(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamMisc.OP

let installed_packages path switch =
  let installed_filename = OpamPath.Switch.installed path switch in
  OpamFile.Installed.safe_read installed_filename

(* Find the installed packages matching a formula *)
let installed_deps formula installed =
  let atoms = OpamFormula.atoms formula in
  (* let package_names = List.map (fun (n, _) -> n) atoms in *)
  let aux acc (n, _) =
    try
      (* let pkg = OpamState.find_installed_package_by_name s n in *)
      let pkg =
        OpamPackage.Set.find (fun p -> OpamPackage.name p = n ) installed
      in
      pkg :: acc
      (* [] *)
    with
      | Not_found -> acc
  in
  List.fold_left aux [] atoms

let opam path nv =
  let opam_f = OpamPath.opam path nv in
  OpamFile.OPAM.safe_read opam_f

(* Find the locally installed mandatory and optional dependencies *)
let local_deps path switch nv =
  let opam = opam path nv in
  let depends = OpamFile.OPAM.depends opam in
  let depopts = OpamFile.OPAM.depopts opam in
  (* Find the installed packages in depends and depopts *)
  let installed = installed_packages path switch in
  installed_deps (OpamFormula.ands [depends; depopts]) installed

(* Retrieve the files installed by a package *)
let files_of_package path switch package =
  let open OpamFilename.OP in
  let name = OpamPackage.name package in
  let filename = OpamPath.Switch.install path switch name in
  let dot_install = OpamFile.Dot_install.safe_read filename in
  let aux_simple prefix acc basename =
    (* Don't include optional files *)
    if basename.optional then acc
    else prefix // (OpamFilename.Base.to_string basename.c) :: acc
  in
  let aux_bin prefix acc (basename, rename_opt) =
    if basename.optional then acc
    else match rename_opt with
      | None -> prefix // (OpamFilename.Base.to_string basename.c) :: acc
      | Some r -> prefix // (OpamFilename.Base.to_string r) :: acc
  in
  let aux_misc prefix acc (basename, filename) =
    if basename.optional then acc
    else prefix // (OpamFilename.to_string filename) :: acc
  in
  List.fold_left (aux_bin (OpamPath.Switch.bin path switch)) []
      (OpamFile.Dot_install.bin dot_install)
  @ List.fold_left (aux_simple (OpamPath.Switch.lib path switch name)) []
      (OpamFile.Dot_install.lib dot_install)
  @ List.fold_left (aux_simple (OpamPath.Switch.toplevel path switch)) []
      (OpamFile.Dot_install.toplevel dot_install)
  @ List.fold_left (aux_simple (OpamPath.Switch.share path switch name)) []
      (OpamFile.Dot_install.share dot_install)
  @ List.fold_left (aux_simple (OpamPath.Switch.doc path switch name)) []
      (OpamFile.Dot_install.doc dot_install)
  @ List.fold_left (aux_misc (OpamPath.Switch.root path switch)) []
      (OpamFile.Dot_install.misc dot_install)

module Digest = struct

  let of_string s =
    Digest.to_hex (Digest.string s)

  (* Return the hex digest of the a concatenated list of strings *)
  let of_list sep strings =
    of_string (String.concat sep strings)

  (* The initial digest used to find an OCaml binary package *)
  let system () =
    let system_config = OpamSystem.ocamlc_config () in
    let os = OpamGlobals.string_of_os (OpamGlobals.os ()) in
    let machine = List.assoc "architecture" system_config in
    let version = List.assoc "version" system_config in
    let source = os ^ machine ^ version in
    of_string source

  (* Create a digest of a compiled package *)
  let rec binary path switch repo_root installed_binaries nv =
    try
      OpamPackage.Name.Map.find (OpamPackage.name nv) installed_binaries
    with
    | Not_found ->
      let env_digest =
        environment path switch repo_root installed_binaries nv in
      (* Checksums of installed files *)
      let sorted_files = List.fast_sort String.compare (
        List.map OpamFilename.to_string (
          files_of_package path switch nv
        )
      ) in
      (* let file_digests = List.map Digest.file files in *)
      (* of_list "" (dep_digests @ file_digests) *)
      (* let files_digest = of_list "" files in *)
      (* of_list "" [files_digest; env_digest] *)
      (* Binary digest of package *)
      of_list "" (env_digest :: sorted_files)

  and of_dependencies path switch repo_root installed_binaries nv =
    (* let opam = opam nv in *)
    (* Get binary digests of dependencies *)
    let deps = local_deps path switch nv in
    List.map (binary path switch repo_root installed_binaries) deps

  (* Create a digest of an environment for a package *)
  and environment path switch repo_root installed_binaries nv =
    (* let opam_f = OpamPath.opam path nv in *)
    let dep_digests =
      of_dependencies path switch repo_root installed_binaries nv in
    (* Get digest of package source archive *)
    (* TODO: get url file checksum if present, generate it otherwise *)
    let archive_digest =
      let url_file =
        let prefix = OpamRepository.read_prefix repo_root in
        let prefix = OpamRepository.find_prefix prefix nv in
        OpamPath.Repository.url repo_root prefix nv in
      if not (OpamFilename.exists url_file) then
        let archive = OpamPath.archive path nv in
        (* TODO: handle digest errors *)
        try OpamFilename.digest archive with _ -> ""
      else
        let url = OpamFile.URL.read url_file in
        match OpamFile.URL.checksum url with
        | None -> ""
        | Some c -> c
    in
    (* Environment digest of package *)
    match dep_digests with
    (* If no dependency, use the system digest *)
    | [] -> of_list "" [archive_digest; system ()]
    | ds -> of_list "" (archive_digest :: ds)

end

let compare_filenames f1 f2 =
  compare (OpamFilename.to_string f1) (OpamFilename.to_string f2)

(*
(* List files in directories listed in [dirs] (prefixed by the root
   directory. If [dirs] is not defined, list files in the root directory *)
let list_files_sorted ?dirs t =
    let open OpamFilename.OP in
    let root_dir = OpamPath.Switch.root t.root t.switch in
    let dirs = match dirs with
      | None -> [ root_dir ]
      | Some ds -> List.map ((/) root_dir) ds
    in
    List.fast_sort compare_filenames
      (List.fold_left (fun accu d -> OpamFilename.rec_files d @ accu) [] dirs)

(* Map files to a list of (file, file_stats) pairs *)
let map_stats files =
  List.map (fun f -> f, Unix.lstat (OpamFilename.to_string f)) files

(* Difference of two list of pairs (file, stats), sorted by [file]
   @return (removed_files * installed_files * modified_files) *)
let diff_filetrees l1 l2 =
  let rec aux rems insts mods r1 r2 = match r1, r2 with
    | h1 :: t1, h2 :: t2 ->
      let eq_stats =
        let st1 = snd h1 in
        let st2 = snd h2 in
        st1.Unix.st_mtime = st2.Unix.st_mtime
          && st1.Unix.st_ino = st2.Unix.st_ino
      in
      let cmp_filename = compare_filenames (fst h1) (fst h2) in
      (* A new file has been installed *)
      if cmp_filename > 0 then aux rems (h2 :: insts) mods r1 t2
      (* A file has been removed *)
      else if cmp_filename < 0 then aux (h1 :: rems) insts mods t1 r2
      (* File has been modified *)
      else if not eq_stats then aux rems insts (h2 :: mods) t1 t2
      (* No change *)
      else aux rems insts mods t1 t2
    (* Leftover files *)
    | [], [] -> List.rev rems, List.rev insts, List.rev mods
    | [], r2 -> List.rev rems, List.rev (r2 @ insts), List.rev mods
    | r1, [] -> List.rev (r1 @ rems), List.rev insts, List.rev mods
  in
  aux [] [] [] l1 l2

*)
