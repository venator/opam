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

let log fmt = OpamGlobals.log "ACTION" fmt

open OpamTypes
open OpamFilename.OP
open OpamState.Types

(* Install the package files *)
(* IMPORTANT: this function is executed by the children processes,
   thus it is important to NOT modify the global state of OPAM here.
   Thus, the update of ~/.opam/<switch/installed MUST not be done
   here.*)
let install_package t nv =
  let build_dir = OpamPath.Switch.build t.root t.switch nv in
  if OpamFilename.exists_dir build_dir then OpamFilename.in_dir build_dir (fun () ->

      OpamGlobals.msg "Installing %s.\n" (OpamPackage.to_string nv);
      let name = OpamPackage.name nv in
      let opam_f = OpamPath.opam t.root nv in
      let opam_ = OpamState.opam t nv in
      let config_f = OpamPath.Switch.build_config t.root t.switch nv in
      let config = OpamFile.Dot_config.safe_read config_f in
      let install_f = OpamPath.Switch.build_install t.root t.switch nv in
      let install = OpamFile.Dot_install.safe_read install_f in

      (* check that libraries and syntax extensions specified in .opam and
         .config are in sync *)
      let check kind config_sections opam_sections =
        List.iter (fun cs ->
          if not (List.mem cs opam_sections) then
            OpamGlobals.error_and_exit "The %s %s does not appear in %s"
              kind (OpamVariable.Section.to_string cs) (OpamFilename.to_string opam_f)
        ) config_sections;
        List.iter (fun os ->
          if not (List.mem os config_sections) then
            OpamGlobals.error_and_exit "The %s %s does not appear in %s"
              kind (OpamVariable.Section.to_string os) (OpamFilename.to_string config_f)
        ) opam_sections in
      if not (OpamFilename.exists config_f)
      && (OpamFile.OPAM.libraries opam_ <> [] || OpamFile.OPAM.syntax opam_ <> []) then
        OpamGlobals.error_and_exit
          "%s does not exist but %s defines some libraries and syntax extensions"
          (OpamFilename.to_string config_f)
          (OpamFilename.to_string opam_f);
      check "library"
        (OpamFile.Dot_config.Library.available config)
        (OpamFile.OPAM.libraries opam_);
      check "syntax"
        (OpamFile.Dot_config.Syntax.available config)
        (OpamFile.OPAM.syntax opam_);

      (* check that depends (in .opam) and requires (in .config) fields
         are in almost in sync *)
      (* NOTES: the check is partial as we don't know which clause is valid
         in depends (XXX there is surely a way to get it from the solver) *)
      let local_sections = OpamFile.Dot_config.Section.available config in
      let libraries_in_opam =
        OpamFormula.fold_left (fun accu (n,_) ->
          if OpamState.mem_installed_package_by_name t n then (
            let nv = OpamState.find_installed_package_by_name t n in
            let opam = OpamState.opam t nv in
            let libs = OpamFile.OPAM.libraries opam in
            let syntax = OpamFile.OPAM.syntax opam in
            List.fold_right OpamVariable.Section.Set.add (libs @ syntax) accu
          ) else
            accu
        ) OpamVariable.Section.Set.empty (OpamFile.OPAM.depends opam_) in
      let libraries_in_config =
        List.fold_left (fun accu s ->
          List.fold_left (fun accu r ->
            OpamVariable.Section.Set.add r accu
          ) accu (OpamFile.Dot_config.Section.requires config s)
        ) OpamVariable.Section.Set.empty local_sections in
      OpamVariable.Section.Set.iter (fun s ->
        if not (List.mem s local_sections)
        && not (OpamVariable.Section.Set.mem s libraries_in_opam) then
          let config_f =
            OpamFilename.to_string (OpamPath.Switch.build_config t.root t.switch nv) in
          let opam_f = OpamFilename.to_string (OpamPath.opam t.root nv) in
          let local_sections =
            List.map OpamVariable.Section.to_string local_sections in
          let opam_sections =
            List.map OpamVariable.Section.to_string
              (OpamVariable.Section.Set.elements libraries_in_opam) in
          OpamGlobals.error_and_exit
            "%s appears as a library dependency in %s, but:\n\
             - %s defines the libraries {%s}\n\
             - Packages in %s defines the libraries {%s}"
            (OpamVariable.Section.to_string s) config_f
            config_f (String.concat ", " local_sections)
            opam_f (String.concat ", " opam_sections)
      ) libraries_in_config;

      (* .install *)
      (* TODO: copy .install if available, instead of generating it ? or copy
         it only in binary mode ? *)
      (* OpamFile.Dot_install.write (OpamPath.Switch.install t.root t.switch name) install; *)

      (* .config *)
      OpamFile.Dot_config.write (OpamPath.Switch.config t.root t.switch name) config;

      let warnings = ref [] in
      let check ~src ~dst base =
        let src_file = OpamFilename.create src base.c in
        if not base.optional && not (OpamFilename.exists src_file) then (
          warnings := (dst, base.c) :: !warnings
        );
        OpamFilename.exists src_file in

      (* Install a list of files *)
      let install_files dst_fn files_fn =
        let dst_dir = dst_fn t.root t.switch name in
        let files = files_fn install in
        if not (OpamFilename.exists_dir dst_dir) then (
          log "creating %s" (OpamFilename.Dir.to_string dst_dir);
          OpamFilename.mkdir dst_dir;
        );
        List.iter (fun (base, dst) ->
            let src_file = OpamFilename.create build_dir base.c in
            let dst_file = match dst with
              | None   -> OpamFilename.create dst_dir (OpamFilename.basename src_file)
              | Some d -> OpamFilename.create dst_dir d in
            if check ~src:build_dir ~dst:dst_dir base then
              OpamFilename.copy ~src:src_file ~dst:dst_file;
          ) files in

      (* bin *)
      install_files (fun r s _ -> OpamPath.Switch.bin r s) OpamFile.Dot_install.bin;

      (* lib *)
      install_files OpamPath.Switch.lib OpamFile.Dot_install.lib;

      (* toplevel *)
      install_files (fun r s _ -> OpamPath.Switch.toplevel r s)
        OpamFile.Dot_install.toplevel;

      install_files (fun r s _ -> OpamPath.Switch.stublibs r s)
        OpamFile.Dot_install.stublibs;

      (* Man pages *)
      install_files (fun r s _ -> OpamPath.Switch.man_dir r s) OpamFile.Dot_install.man;

      (* Shared files *)
      install_files OpamPath.Switch.share OpamFile.Dot_install.share;

      (* Documentation files *)
      install_files OpamPath.Switch.doc OpamFile.Dot_install.doc;

      (* misc *)
      List.iter
        (fun (src, dst) ->
          let src_file = OpamFilename.create (OpamFilename.cwd ()) src.c in
          if OpamFilename.exists dst
          && OpamState.confirm "Overwriting %s ?" (OpamFilename.to_string dst) then
            OpamFilename.copy ~src:src_file ~dst
          else begin
            OpamGlobals.msg "Installing %s to %s.\n"
              (OpamFilename.Base.to_string src.c) (OpamFilename.to_string dst);
            if OpamState.confirm "Continue ?" then
              OpamFilename.copy ~src:src_file ~dst
          end
        ) (OpamFile.Dot_install.misc install);

      if !warnings <> [] then (
        let print (dir, base) =
          Printf.sprintf " - %s in %s"
            (OpamFilename.Base.to_string base)
            (OpamFilename.Dir.to_string dir) in
        OpamSystem.internal_error
          "Error while installing the following files:\n%s"
          (String.concat "\n" (List.map print !warnings));
      )
    );
  if not (!OpamGlobals.keep_build_dir || !OpamGlobals.debug) then
    OpamFilename.rmdir build_dir

let get_archive t nv =
  log "get_archive %s" (OpamPackage.to_string nv);
  let dst = OpamPath.archive t.root nv in
  if OpamFilename.exists dst then Some dst
  else
    match OpamState.package_repository_state t nv with
    | None   -> None
    | Some s ->
      begin match s.pkg_archive with
        | None   -> OpamRepository.download s.pkg_repo nv
        | Some _ -> ()
      end;
      let src = OpamPath.Repository.archive s.pkg_repo nv in
      if OpamFilename.exists src then (
        OpamFilename.move ~src ~dst;
        Some dst;
      ) else
        None

(* Prepare the package build:
   * apply the patches
   * substitute the files *)
let prepare_package_build t nv =
  let p_build = OpamPath.Switch.build t.root t.switch nv in
  let opam = OpamState.opam t nv in

  if not (OpamFilename.exists_dir p_build) then
    OpamFilename.mkdir p_build;

  (* Substitute the patched files.*)
  let patches = OpamFile.OPAM.patches opam in
  OpamFilename.in_dir p_build (fun () ->
    let all = OpamFile.OPAM.substs opam in
    let patches =
      OpamMisc.filter_map (fun (f,_) ->
        if List.mem f all then Some f else None
      ) patches in
    List.iter (OpamState.substitute_file t) patches
  );

  (* Apply the patches *)
  List.iter (fun (base, filter) ->
    let root = OpamPath.Switch.build t.root t.switch nv in
    let patch = root // OpamFilename.Base.to_string base in
    if OpamState.eval_filter t filter then (
      OpamGlobals.msg "Applying %s.\n" (OpamFilename.Base.to_string base);
      OpamFilename.patch patch p_build)
  ) patches;

  (* Substitute the configuration files. We should be in the right
     directory to get the correct absolute path for the
     substitution files (see [substitute_file] and
     [OpamFilename.of_basename]. *)
  OpamFilename.in_dir p_build (fun () ->
    List.iter (OpamState.substitute_file t) (OpamFile.OPAM.substs opam)
  )

(* For pinned packages, we keep the build cache in
   ~/.opam/<switch>/pinned.cache/<name> as:
   i) it's quite important to build and sync-up in different places
   ii) the build directory will be deleted afterwards anyway *)
let extract_package t nv =
  log "extract_package: %s" (OpamPackage.to_string nv);
  let build_dir = OpamPath.Switch.build t.root t.switch nv in
  OpamFilename.rmdir build_dir;
  begin match OpamState.pinned_path t (OpamPackage.name nv) with
    | Some p ->

      let pinned_dir =
        OpamPath.Switch.pinned_dir t.root t.switch (OpamPackage.name nv) in
      if not (OpamFilename.exists_dir pinned_dir) then (
        match OpamState.update_pinned_package t (OpamPackage.name nv) with
        | Not_available -> OpamGlobals.error "%s is not available"
                             (OpamFilename.Dir.to_string p)
        | Result _
        | Up_to_date _  -> ()
      ) else
        OpamGlobals.msg
          "Synchronization: nothing to do as the pinned package has already \
           been initialized.\n";

      begin (* Copy eventual files *)
        try
          let repo_name = OpamPackage.Map.find nv t.package_index in
          let repo = OpamRepositoryName.Map.find repo_name t.repositories in
          let _files = OpamFilename.in_dir pinned_dir (fun () ->
            OpamRepository.copy_files repo nv
          ) in ()
        with Not_found ->
          ()
      end;

      (* Copy the resulting dir *)
      OpamFilename.copy_dir ~src:pinned_dir ~dst:build_dir

    | None ->
      match get_archive t nv with
      | None         -> ()
      | Some archive ->
        OpamGlobals.msg "Extracting %s.\n" (OpamFilename.to_string archive);
        OpamFilename.extract archive build_dir
  end;

  prepare_package_build t nv;
  build_dir

let string_of_commands commands =
  let commands_s = List.map (fun cmd -> String.concat " " cmd)  commands in
  "  "
  ^ if commands_s <> [] then
    String.concat "\n  " commands_s
  else
    "Nothing to do."

let compilation_env t opam =
  let env0 = OpamState.get_full_env t in
  let env1 = [
    ("OPAM_PACKAGE_NAME", OpamPackage.Name.to_string (OpamFile.OPAM.name opam));
    ("OPAM_PACKAGE_VERSION", OpamPackage.Version.to_string (OpamFile.OPAM.version opam))
  ] @ env0 in
  OpamState.add_to_env t env1 (OpamFile.OPAM.build_env opam)

let get_metadata t =
  let compiler =
    if t.compiler = OpamCompiler.system then
      let system_version = match OpamCompiler.Version.system () with
        | None   -> "<none>"
        | Some v -> OpamCompiler.Version.to_string v in
      Printf.sprintf "system (%s)" system_version
    else
      OpamCompiler.to_string t.compiler in
  [
    ("compiler", compiler);
  ]

let update_metadata t ~installed ~installed_roots ~reinstall
    ~installed_binaries =
  let installed_roots = OpamPackage.Set.inter installed_roots installed in
  let reinstall = OpamPackage.Set.inter installed_roots reinstall in
  OpamFile.Installed.write
    (OpamPath.Switch.installed t.root t.switch)
    installed;
  OpamFile.Installed_roots.write
    (OpamPath.Switch.installed_roots t.root t.switch)
    installed_roots;
  OpamFile.Reinstall.write
    (OpamPath.Switch.reinstall t.root t.switch)
    reinstall;
  OpamFile.Installed_binaries.write
    (OpamPath.Switch.installed_binaries t.root t.switch)
    installed_binaries

(* Remove a given package *)
(* This will be done by the parent process, so theoritically we are
   allowed to modify the global state of OPAM here. However, for
   consistency reasons, this is done in the main function only. *)
let remove_package_aux t ~metadata ~rm_build nv =
  log "Removing %s (%b)" (OpamPackage.to_string nv) metadata;
  let name = OpamPackage.name nv in

  (* Run the remove script *)
  let opam_f = OpamPath.opam t.root nv in
  OpamGlobals.msg "Removing %s.\n" (OpamPackage.to_string nv);

  if not (OpamFilename.exists opam_f) then
    OpamGlobals.msg "  No OPAM file has been found!\n"
  else (
    let opam = OpamState.opam t nv in
    let env = compilation_env t opam in
    match OpamState.filter_commands t (OpamFile.OPAM.remove opam) with
    | []     -> ()
    | remove ->
      let p_build = OpamPath.Switch.build t.root t.switch nv in
      (* We try to run the remove scripts in the folder where it was
         extracted If it does not exist, we try to download and
         extract the archive again, if that fails, we don't really
         care. *)
      (* We also use a small hack: if the remove command is simply
         'ocamlfind remove xxx' then, no need to extract the archive
         again. *)
      let use_ocamlfind = function
        | [] -> true
        | "ocamlfind" :: _ -> true
        | _ -> false in
      if not (OpamFilename.exists_dir p_build)
      && not (List.for_all use_ocamlfind remove) then (
        try let _ = extract_package t nv in ()
        with _ -> ()
      );
      let name = OpamPackage.Name.to_string name in
      let exec_dir, name =
        if OpamFilename.exists_dir p_build
        then p_build, Some name
        else t.root , None in
      try
        OpamGlobals.msg "%s\n" (string_of_commands remove);
        let metadata = get_metadata t in
        OpamFilename.exec ~env ?name exec_dir ~metadata remove
      with _ ->
        ();
  );

  (* Remove the libraries *)
  OpamFilename.rmdir (OpamPath.Switch.lib t.root t.switch name);

  (* Remove the documentation *)
  OpamFilename.rmdir (OpamPath.Switch.doc t.root t.switch name);
  (* XXX: remove the man pages *)

  (* Remove build/<package> if requested *)
  if not !OpamGlobals.keep_build_dir && rm_build then
    OpamFilename.rmdir (OpamPath.Switch.build t.root t.switch nv);

  (* Clean-up the active repository *)
  log "Cleaning-up the active repository";
  begin
    try
      let repo_name = OpamPackage.Map.find nv t.package_index in
      let repo = OpamRepositoryName.Map.find repo_name t.repositories in
      let tmp_dir = OpamPath.Repository.tmp_dir repo nv in
      OpamFilename.rmdir tmp_dir
    with Not_found ->
      ()
  end;

  let install =
    OpamFile.Dot_install.safe_read (OpamPath.Switch.install t.root t.switch name) in

  let remove_files dst_fn files =
    let files = files install in
    List.iter (fun (base, dst) ->
        let dst_dir = dst_fn t.root t.switch in
        let dst_file = match dst with
          | None   -> OpamFilename.create dst_dir base.c
          | Some b -> OpamFilename.create dst_dir b in
        OpamFilename.remove dst_file
      ) files in

  (* Remove the binaries *)
  log "Removing the binaries";
  remove_files OpamPath.Switch.bin OpamFile.Dot_install.bin;

  (* Remove the C bindings *)
  remove_files OpamPath.Switch.stublibs OpamFile.Dot_install.stublibs;

  (* Remove man pages *)
  remove_files OpamPath.Switch.man_dir OpamFile.Dot_install.man;

  (* Remove the misc files *)
  log "Removing the misc files";
  List.iter (fun (_,dst) ->
    if OpamFilename.exists dst then begin
      OpamGlobals.msg "Removing %s." (OpamFilename.to_string dst);
      if OpamState.confirm "Continue ?" then
        OpamFilename.remove dst
    end
  ) (OpamFile.Dot_install.misc install);

  (* Removing the shared dir if it is empty, overwise keep files for
     future installation. TODO: is it the expected semantics ? *)
  let share = OpamPath.Switch.share t.root t.switch name in
  (match OpamFilename.rec_files share, OpamFilename.rec_dirs share with
   | [], [] -> OpamFilename.rmdir share
   | _      ->
     OpamGlobals.msg
       "WARNING: %s is not empty. We keep its contents for future installations.\n"
       (OpamFilename.Dir.to_string share));

  (* Remove .config and .install *)
  log "Removing config and install files";
  OpamFilename.remove (OpamPath.Switch.install t.root t.switch name);
  OpamFilename.remove (OpamPath.Switch.config t.root t.switch name);

  (* Remove the pinned cache *)
  log "Removing the pinned cache";
  OpamFilename.rmdir (OpamPath.Switch.pinned_dir t.root t.switch name);

  (* Update the metadata *)
  if metadata then (
    let installed = OpamPackage.Set.remove nv t.installed in
    let installed_roots = OpamPackage.Set.remove nv t.installed_roots in
    let reinstall = OpamPackage.Set.remove nv t.reinstall in
    let installed_binaries = OpamPackage.Name.Map.remove
        (OpamPackage.name nv) t.installed_binaries in
    update_metadata t ~installed ~installed_roots ~reinstall
      ~installed_binaries
  )

let remove_package t ~metadata ~rm_build nv =
  (* TODO: use the generated .install file to uninstall a package, instead of
     relying on uncontrollable ways of uninstalling *)
  if not !OpamGlobals.fake then
    remove_package_aux t ~metadata ~rm_build nv
  else
    OpamGlobals.msg "(simulation) Removing %s.\n" (OpamPackage.to_string nv)

(* Remove all the packages appearing in a solution (and which need to
   be removed, eg. because of a direct uninstall action or because of
   recompilation.  *)
let remove_all_packages t ~metadata sol =
  let open PackageActionGraph in
  let deleted = ref [] in
  let delete nv =
    if !deleted = [] then
      OpamGlobals.msg "\n=-=-= Removing Packages =-=-=\n";
    deleted := nv :: !deleted;
    try remove_package t ~rm_build:true ~metadata:false nv;
    with _ -> () in
  let action n =
    match n with
    | To_change (Some nv, _)
    | To_recompile nv
    | To_delete nv        -> delete nv
    | To_change (None, _) -> () in
  List.iter delete sol.to_remove;
  PackageActionGraph.iter_vertex action (PackageActionGraph.mirror sol.to_process);
  let deleted = OpamPackage.Set.of_list !deleted in
  if metadata then (
    let installed = OpamPackage.Set.diff t.installed deleted in
    let installed_roots = OpamPackage.Set.diff t.installed_roots deleted in
    let reinstall = OpamPackage.Set.diff t.reinstall deleted in
    let installed_binaries = OpamPackage.Set.fold (fun nv map ->
        OpamPackage.Name.Map.remove (OpamPackage.name nv) map)
      deleted t.installed_binaries in
    update_metadata t ~installed ~installed_roots ~reinstall
        ~installed_binaries
  );
  deleted

(* Use the 'whereis' command to check if all external libs are available *)
let check_extlib t nv env_id bin_id =
  let extlib_file = OpamPath.binary_extlib t nv env_id bin_id in
  let extlib_set = OpamFile.Package_extlib.safe_read extlib_file in
  OpamPackage.Extlib.Set.for_all (fun l ->
      List.length (OpamSystem.whereis (OpamPackage.Extlib.to_string l)) > 0)
    extlib_set

(* Look for a binary package corresponding to the given environment digest in
   known repositories. *)
let find_binary t nv env_id =
  let binary_dir = OpamPath.binary_dir t.root nv env_id in
  if not (OpamFilename.exists_dir binary_dir) then
    raise Not_found;
  let binary_files = OpamFilename.rec_files binary_dir in
  let rec aux = function
    | [] -> raise Not_found
    | f :: r ->
      if OpamFilename.check_suffix f ".tar.gz" then
        let f_str = OpamFilename.to_string f in
        let no_ext = OpamMisc.remove_suffix ~suffix:".tar.gz" f_str in
        match OpamMisc.rcut_at no_ext '+' with
        | None -> aux r
        | Some (_, checksum) ->
          if check_extlib t.root nv env_id checksum then
            f, checksum
          else
            aux r
      else aux r
  in
  aux binary_files

(* Same as [find_binary], but returning an optional value
   @return None if no binary package has been found *)
let find_binary_opt t nv env_id =
  try Some (find_binary t nv env_id) with Not_found -> None

(* Compare two [OpamFilename.t] using their string representation *)
let compare_filenames f1 f2 =
  compare (OpamFilename.to_string f1) (OpamFilename.to_string f2)

(* Map files to a list of (file, file_stats) pairs *)
let map_stats files =
  List.map (fun f -> f, Unix.lstat (OpamFilename.to_string f)) files

(* Debug function used to print the list of installed files *)
(*
let print_diff (removed_files, installed_files, modified_files) =
    let print_filenames header files =
      Printf.printf "\n===== %s =====\n" header;
      if List.length files > 0 then
        List.iter (fun f -> print_endline (OpamFilename.to_string f)) files
      else
        print_endline "None"
    in

    print_filenames "Removed files" removed_files;
    print_filenames "Installed files" installed_files;
    print_filenames "Modified files" modified_files
*)

(* Create a log file associated to a binary package.
   It contains the environment parameters as returned by 'ocamlc -config' *)
let log_package filename =
  let ocamlc_config = OpamSystem.ocamlc_config () in
  let log_content =
    List.fold_left (fun acc (k, v) -> Printf.sprintf "%s%s: %s\n" acc k v) ""
        ocamlc_config in
  let log_filename =
    (OpamMisc.remove_suffix ~suffix:".tar.gz" filename) ^ ".config" in
  OpamSystem.write log_filename log_content

(* Create a binary package, that is to say a .tar.gz archive containing all
   the files installed by OPAM for a given package *)
let archive_package t nv env_id bin_id installed_files =
  (* Make binary package filename *)
  let binary_file = OpamPath.binary t.root nv env_id bin_id in
  let filename = OpamFilename.to_string binary_file in

  OpamGlobals.msg "Creating package %s.\n" filename;

  (* Create the binary package directory *)
  let binary_dir = OpamPath.binary_dir t.root nv env_id in

  if not (OpamFilename.exists_dir binary_dir) then
    OpamFilename.mkdir binary_dir;

  (* Archive pathes from the switch root dir *)
  let prefix = OpamPath.Switch.root t.root t.switch in
  let files = List.map (fun f -> OpamFilename.remove_prefix prefix f)
      installed_files
  in

  (* Archive the files in a package *)
  match files with
  | [] -> OpamGlobals.error "No new file installed, unable to create package"
  | _ ->
    OpamSystem.archive filename files (OpamFilename.Dir.to_string prefix);
    (* Create a log file associated to the package *)
    log_package filename

(* Difference of two list of pairs (file, stats), sorted by [file]
   @return (removed_files * installed_files * modified_files) *)
let diff_filetrees l1 l2 =
  let rec aux rems insts mods r1 r2 = match r1, r2 with
    | (f1, st1) :: t1, (f2, st2) :: t2 ->
      let eq_stats =
        st1.Unix.st_mtime = st2.Unix.st_mtime
          && st1.Unix.st_ino = st2.Unix.st_ino
      in
      let cmp_filename = compare_filenames f1 f2 in
      (* A new file has been installed *)
      if cmp_filename > 0 then aux rems (f2 :: insts) mods r1 t2
      (* A file has been removed *)
      else if cmp_filename < 0 then aux (f1 :: rems) insts mods t1 r2
      (* File has been modified *)
      else if not eq_stats then aux rems insts (f2 :: mods) t1 t2
      (* No change *)
      else aux rems insts mods t1 t2
    (* End of lists *)
    | [], [] -> List.rev rems, List.rev insts, List.rev mods
    (* Leftover files *)
    | [], (f, _) :: r -> aux rems (f :: insts) mods r1 r
    | (f, _) :: r, [] -> aux (f :: rems) insts mods r r2
  in
  aux [] [] [] l1 l2

(* Difference of two lists of elements which can be sorted with [compare] *)
let diff_list compare l1 l2 =
  let rec aux acc r1 r2 = match r1, r2 with
    | [], [] -> List.rev acc
    | [], r | r, []  -> List.rev ((List.rev r) @ acc)
    | h1 :: t1, h2 :: t2 ->
      if compare h1 h2 > 0 then aux (h2 :: acc) r1 t2
      else if compare h1 h2 < 0 then aux (h1 :: acc) t1 r2
      else aux acc t1 t2
  in
  let sort = List.fast_sort compare in
  aux [] (sort l1) (sort l2)

(* List files in directories listed in [dirs] (prefixed by the root
   directory. If [dirs] is not defined, list files in the root directory *)
let list_files_sorted ?dirs t =
  let open OpamFilename.OP in
  let root_dir = OpamPath.Switch.root t.root t.switch in
  let dirs = match dirs with
    | None -> [ ("", root_dir) ]
    | Some ds -> ds
  in
  List.fast_sort compare_filenames
    (List.fold_left (fun acc (_, d) -> acc @ OpamFilename.rec_files d) [] dirs)

(* Sort a list of files by their prefix directory. If the prefix directory is 
   not in the given [dirs] list, the file is added to the leftover file list.
   @return ((directory * files) * leftover files) *)
let sort_files_in_dirs ?(dirs = []) files =
  let acc_opt (id, _) l acc =
    if List.length l > 0 then (id, l) :: acc else acc
  in
  let rec aux acc acc_files acc_misc ds fs =
    match ds, fs with
    | [], _ -> acc_files, ((List.rev fs) @ acc_misc)
    | d :: _, [] -> acc_opt d acc acc_files, acc_misc
    | (id, d) :: dt, f :: ft ->
      if OpamFilename.starts_with d f then
        aux (f :: acc) acc_files acc_misc ds ft
      else if (OpamFilename.to_string f) > (OpamFilename.Dir.to_string d) then
        aux [] (acc_opt (id, d) acc acc_files) acc_misc dt fs
      else aux acc acc_files (f :: acc_misc) ds ft
  in
  aux [] [] [] dirs files

(* Create a [OpamFile.Dot_install.t] using files sorted by their prefix
   directory, as returned by [sort_files_in_dirs]. *)
let dot_install_of_files (file_in_dirs, misc_files) =
  let str_of_filenames = List.map OpamFilename.to_string in
  let file_in_dirs_str =
    List.map (fun (s, fs) -> s, str_of_filenames fs) file_in_dirs
  in
  let misc_files_str = str_of_filenames misc_files in
  let assoc dir =
    try List.assoc dir file_in_dirs_str with Not_found -> []
  in
  let to_basename_aux f =
    {optional = false; c = OpamFilename.Base.of_string f} in
  let to_basename f = (to_basename_aux f, None) in
  let to_basename_misc f = (to_basename_aux f, OpamFilename.of_string f) in
  OpamFile.Dot_install.create
      ~bin: (List.map to_basename (assoc "bin"))
      ~lib: (List.map to_basename (assoc "lib"))
      ~toplevel: (List.map to_basename (assoc "toplevel"))
      ~stublibs: (List.map to_basename (assoc "stublibs"))
      ~share: (List.map to_basename (assoc "share"))
      ~doc: (List.map to_basename (assoc "doc"))
      ~man: (List.map to_basename (assoc "man"))
      ~misc: (List.map to_basename_misc misc_files_str)
      ()

(* Write an [OpamFile.Dot_install.t] to a file *)
let write_dot_install t nv dot_install =
  let name = OpamPackage.name nv in
  OpamFile.Dot_install.write
      (OpamPath.Switch.install t.root t.switch name) dot_install

(* Returns native binaries among installed files *)
let binaries_of_installed_files (file_in_dirs, _) =
  try List.assoc "bin" file_in_dirs with Not_found -> []

(* A list associating install directories to their absolute path in the current
   OPAM switch. *)
let install_dirs t nv =
  let n = OpamPackage.name nv in
  [
    ("bin", OpamPath.Switch.bin t.root t.switch);
    ("lib", OpamPath.Switch.lib t.root t.switch n);
    ("toplevel", OpamPath.Switch.toplevel t.root t.switch);
    ("stublibs", OpamPath.Switch.stublibs t.root t.switch);
    ("share", OpamPath.Switch.share t.root t.switch n);
    ("doc", OpamPath.Switch.doc t.root t.switch n);
    ("man", OpamPath.Switch.man_dir t.root t.switch);
  ]

(* Build and install a package. In case of error, simply return the
   error traces, and let the repo in a state that the user can
   explore.  Do not try to recover yet. *)
let build_and_install_package_aux t ~metadata nv =
  let left, right = match !OpamGlobals.utf8_msgs with
    | true -> "\xF0\x9F\x90\xAB " (* UTF-8 <U+1F42B, U+0020> *), ""
    | false -> "=-=-=", "=-=-="
  in
  OpamGlobals.msg "\n%s Installing %s %s\n" left (OpamPackage.to_string nv) right;

  let opam = OpamState.opam t nv in

  (* Get the env variables set up in the compiler description file *)
  let env = compilation_env t opam in

  (* Get the package state *)
  let pkg_state_opt = OpamState.package_repository_state t nv in

  try

    let env_checksum, binary_package_opt =
      match !OpamGlobals.binary, pkg_state_opt with
      | true, Some pkg_state ->
        let checksum =
          OpamBinary.Digest.environment t.root t.switch pkg_state.pkg_repo
              t.installed_binaries nv in
        checksum, find_binary_opt t nv checksum
      | false, _
      | _, None -> "", None
    in

    let dirs = install_dirs t nv in

    (* [bin_info] is an optional pair of (bin_checksum * dot_install_files) *)
    let bin_info = match binary_package_opt with

    (* A binary package has been found *)
    | Some (bin_pkg, bin_checksum) -> (
      let open OpamFilename.OP in
      let bin_pkg_str = OpamFilename.to_string bin_pkg in
      OpamGlobals.msg "Extracting binary package.\n";
      let switch_root = OpamPath.Switch.root t.root t.switch in
      let switch_root_str = OpamFilename.Dir.to_string switch_root in
      let installed_files = List.map (fun f -> switch_root // f)
        (OpamSystem.list_archive bin_pkg_str switch_root_str) in
      let dot_install_files = sort_files_in_dirs ~dirs installed_files in
      OpamSystem.extract_in bin_pkg_str switch_root_str;
      Some (bin_checksum, dot_install_files)
    )

    (* No binary package has been found, build it *)
    | None -> (

      let init_filetree =
        if !OpamGlobals.binary then
          map_stats (list_files_sorted t)
        else
          []
      in

      (* This one can raises an exception (for insance an user's CTRL-C
         when the sync takes too long. *)
      let p_build = extract_package t nv in

      (* Exec the given commands. *)
      let exec name f =
        match OpamState.filter_commands t (f opam) with
        | []       -> ()
        | commands ->
          OpamGlobals.msg "%s:\n%s\n" name (string_of_commands commands);
          let name = OpamPackage.Name.to_string (OpamPackage.name nv) in
          let metadata = get_metadata t in
          OpamFilename.exec ~env ~name ~metadata p_build commands in

      (* First, we build the package. *)
      exec ("Building " ^ OpamPackage.to_string nv) OpamFile.OPAM.build;

      (* If necessary, build and run the test. *)
      if !OpamGlobals.build_test then
        exec "Building and running the test" OpamFile.OPAM.build_test;

      (* If necessary, build the documentation. *)
      if !OpamGlobals.build_doc then
        exec "Generating the documentation" OpamFile.OPAM.build_doc;

      (* If everyting went fine, finally install the package. *)
      install_package t nv;

      if !OpamGlobals.binary then (
        (* New list of files in the installation dir. *)
        let new_filetree = map_stats (list_files_sorted t) in
        (* let new_filetree = map_stats (list_files ~dirs sorted t) in *)

        (* Find removed, installed and modified files. *)
        let (removed_files, installed_files, modified_files) =
          diff_filetrees init_filetree new_filetree
        in

        (* Exit with an error if some files have been removed *)
        if List.length removed_files > 0 then begin
          OpamGlobals.error_and_exit
              "Some files have been removed during the installation."
        end;

        (* Issue a warning and don't create the package if some files have been 
           modified *)
        if List.length modified_files > 0 then begin
          OpamGlobals.warning "Some files have been modified during the installation, unable to create a binary package.";
          None
        end
        else begin

          (* Create a binary package. *)
          match pkg_state_opt with
          | None -> None
          | Some pkg_state ->
            let bin_checksum =
              OpamBinary.Digest.binary t.root t.switch pkg_state.pkg_repo
                  t.installed_binaries nv in
            archive_package t nv env_checksum bin_checksum installed_files;

            (* Call ldd on binaries and write the extlib file *)
            (* TODO: check if binaries exists outside of $OPAM/$OVERSION/bin *)
            let dot_install_files = sort_files_in_dirs ~dirs installed_files in
            let binaries = binaries_of_installed_files dot_install_files in
            let natives = OpamBinary.filter_natives binaries in
            let ldd = OpamBinary.ldd_of_files natives in
            OpamFile.Package_extlib.write
                (OpamPath.binary_extlib t.root nv env_checksum bin_checksum)
                (OpamBinary.extlib_of_ldd ldd);

            Some (bin_checksum, dot_install_files)
          end
      )
      (* The package hasn't been installed from a pre-compiled archive,
         and no pre-compiled archive has been saved for later use *)
      else
        None

    ) in

    match bin_info with
    | None -> ()
    | Some (_, dot_install_files) ->
      (* Create a .install containing installed files. *)
      let install = dot_install_of_files dot_install_files in
      write_dot_install t nv install;

    (* update the metadata *)
    if metadata then (
      let installed = OpamPackage.Set.add nv t.installed in
      let installed_roots = OpamPackage.Set.add nv t.installed_roots in
      let reinstall = OpamPackage.Set.remove nv t.reinstall in
      let installed_binaries =
        match bin_info with
        | None -> t.installed_binaries
        | Some (c, _) ->
          OpamPackage.Name.Map.add (OpamPackage.name nv) c t.installed_binaries
      in
      update_metadata t ~installed ~installed_roots ~reinstall
          ~installed_binaries
    )

  with e ->
    let cause = match e with
      | Sys.Break -> "was aborted"
      | _         -> "failed" in
    (* We keep the build dir to help debugging *)
    OpamGlobals.error
      "The compilation of %s %s."
      (OpamPackage.to_string nv) cause;
    remove_package ~rm_build:false ~metadata:false t nv;
    raise e

let build_and_install_package t ~metadata nv =
  if not !OpamGlobals.fake then
    (* Proceed to build and install *)
    build_and_install_package_aux t ~metadata nv
  else
    OpamGlobals.msg "(simulation) Building and installing %s.\n"
      (OpamPackage.to_string nv)
