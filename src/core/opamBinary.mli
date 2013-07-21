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

(** Helper functions to handle binary packages *)

(** {2 Ldd} **)

module Ldd: sig

  (** A list of ldd outputs ([binary_file] * [extlib_set]) *)
  type t

  (** Filter a list of binary files to keep only native binaries *)
  val filter_natives: filename list -> filename list

  (** Get dynamic libraries on which depend a list of native binaries *)
  val of_files: filename list -> t

  (** Get an external librarires set out of a ldd output *)
  val to_extlib: t -> extlib_set

end

(** {2 Binary packages digests} **)

module Digest: sig

  (* A digest *)
  type t

  val of_string: string -> t

  val to_string: t -> string

  (* A package digest which can be calculated when the package is installed *)
  val binary:
      dirname -> switch -> repository -> string name_map -> package -> t

  (* A package digest which can be calculated before the package is installed *)
  val environment:
      dirname -> switch -> repository -> string name_map -> package -> t

end
