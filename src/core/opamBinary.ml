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

module Ldd = struct

  type t = (string * extlib_set) list

  (* Filter out bytecode binaries *)
  let filter_natives bin_files =
    List.filter (fun bin ->
        let bin_str = OpamFilename.to_string bin in
        if OpamMisc.ends_with ~suffix:".byte" bin_str then false else true)
      bin_files

  (* Call ldd on binary files *)
  let of_files natives =
    List.map (fun native ->
        let native_str = OpamFilename.to_string native in
        let ldd = OpamSystem.ldd native_str in
        let libs = List.map OpamPackage.Extlib.of_string ldd in
        (native_str, OpamPackage.Extlib.Set.of_list libs))
      natives

  (* Returns a set of external dependencies given a ldd output *)
  let to_extlib ldd =
    List.fold_left (fun acc (_, s) -> OpamPackage.Extlib.Set.union acc s)
        OpamPackage.Extlib.Set.empty ldd

end

(* Retrieve the files installed by a package *)
let files_of_package path switch package =
  let open OpamFilename.OP in
  let name = OpamPackage.name package in
  let filename = OpamPath.Switch.install path switch name in

  let dot_install = OpamFile.Dot_install.safe_read filename in
  let aux acc (basename, rename_opt) =
    if basename.optional then acc
    else match rename_opt with
      | None -> OpamFilename.of_string
          (OpamFilename.Base.to_string basename.c) :: acc
      | Some r -> OpamFilename.of_string
          (OpamFilename.Base.to_string r) :: acc in
  let aux_misc acc (basename, filename) =
    if basename.optional then acc
    else filename :: acc in
  let binaries =
    List.fold_left aux [] (OpamFile.Dot_install.bin dot_install) in
  let libs =
    List.fold_left aux [] (OpamFile.Dot_install.lib dot_install) in
  let toplevels =
    List.fold_left aux [] (OpamFile.Dot_install.toplevel dot_install) in
  let stublibs =
    List.fold_left aux [] (OpamFile.Dot_install.stublibs dot_install) in
  let shares =
    List.fold_left aux [] (OpamFile.Dot_install.share dot_install) in
  let docs =
    List.fold_left aux [] (OpamFile.Dot_install.doc dot_install) in
  let mans =
    List.fold_left aux [] (OpamFile.Dot_install.man dot_install) in
  let extlibs =
    Ldd.to_extlib (Ldd.of_files (Ldd.filter_natives binaries)) in
  binaries @ libs @ toplevels @ stublibs @ shares @ docs @ mans
  @ List.fold_left aux_misc [] (OpamFile.Dot_install.misc dot_install),
  (OpamPackage.Extlib.Set.elements extlibs)

module Digest = struct

  type t = string

  let of_string s =
    Digest.to_hex (Digest.string s)

  let to_string d = d

  (* Return the hex digest of the a concatenated list of strings *)
  let of_list sep strings =
    of_string (String.concat sep strings)

  (* Get the set of installed packages *)
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

  (* Load the opam file associated to a package *)
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
      let installed_pathes, extlibs = files_of_package path switch nv in
      (* Remove system specific path *)
      let switch_path = OpamPath.Switch.root path switch in
      let installed_files = List.map (fun p ->
          OpamFilename.remove_prefix switch_path p)
        installed_pathes in
      (* Checksums of installed files *)
      let sorted_files = List.fast_sort String.compare installed_files in
      List.iter print_endline sorted_files;
      let extlib_str = List.map OpamPackage.Extlib.to_string extlibs in
      (* Binary digest of package *)
      of_list "" (env_digest :: extlib_str @ sorted_files)

  and of_dependencies path switch repo_root installed_binaries nv =
    (* let opam = opam nv in *)
    (* Get binary digests of dependencies *)
    let deps = local_deps path switch nv in
    List.map (binary path switch repo_root installed_binaries) deps

  (* Create a digest of an environment for a package *)
  and environment path switch repo_root installed_binaries nv =
    (* Dependency checksums *)
    let dep_digests =
      of_dependencies path switch repo_root installed_binaries nv in
    (* Get digest of package source archive *)
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
