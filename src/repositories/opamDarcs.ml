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
open OpamFilename.OP

let log fmt = OpamGlobals.log "Darcs" fmt

module Darcs = struct

  let exists repo =
    OpamFilename.exists_dir (repo.repo_root / "_darcs")

  let init repo =
    let repo = OpamFilename.Dir.to_string repo.repo_address in
    OpamSystem.commands [
      [ "darcs" ; "init" ];
      [ "darcs" ; "get" ; repo; "--lazy" ];
    ]

  (* With darcs, it is apparently easier to compute a diff between
     remote and local, without fething at all. So we set fetch to be a
     no-op. *)
  let fetch _ =
    ()

  (* Merge is actually a full pull *)
  let merge repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
        let repo = OpamFilename.Dir.to_string repo.repo_address in
        OpamSystem.command [
          "darcs" ; "pull"; repo; "--all"; "--quiet"
        ];
    )

  (* Difference between remote and local is a 'pull --dry-run' *)
  let diff repo =
    OpamFilename.in_dir repo.repo_root (fun () ->
        let repo = OpamFilename.Dir.to_string repo.repo_address in
        let patches =
          OpamSystem.read_command_output [
            "darcs" ; "pull" ; repo; "--dry-run" ; "--quiet"
          ] in
        patches <> []
      )

  let revision _ =
    "<darcs-???>"

end

module B = OpamVCS.Make(Darcs)

let register () =
  OpamRepository.register_backend `darcs (module B: OpamRepository.BACKEND)
