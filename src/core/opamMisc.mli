(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

(** Basic functions *)

(** {2 Abstract types} *)

(** Collection of abstract values *)
module type SET = sig

  include Set.S

  (** auto-map *)
  val map: (elt -> elt) -> t -> t

  (** Return one element. Fail if the set is not a singleton. *)
  val choose_one : t -> elt

  (** Make a set from a list *)
  val of_list: elt list -> t

  (** Pretty-print a set *)
  val to_string: t -> string

  (** Find an element in the list *)
  val find: (elt -> bool) -> t -> elt

end

(** Dictionaries of abstract values *)
module type MAP = sig

  include Map.S

  (** Pretty-printing *)
  val to_string: ('a -> string) -> 'a t  -> string

  (** Return the values in the map. *)
  val values: 'a t -> 'a list

  (** Return the keys in the map. *)
  val keys: 'a t -> key list

  (** Same as [merge] but only keys that appear in both maps
      are given in the merging function *)
  (** WARNING : Besides [key], the function could receive
      some [v1] and some [v2] such that [v1 = v2] holds. *)
  val merge_max: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  (** Convert an assoc list to a map *)
  val of_list: (key * 'a) list -> 'a t

end

(** All abstract types should implement this signature *)
module type ABSTRACT = sig

  (** ABSTRACT type *)
  type t

  (** Create an abstract value from a string *)
  val of_string: string -> t

  (** Convert an abstract value to a string *)
  val to_string: t -> string

  module Set: SET with type elt = t
  module Map: MAP with type key = t
end

(** Extended sets and maps *)
module type OrderedType = sig
  include Set.OrderedType
  val to_string: t -> string
end

(** Set constructor *)
module Set: sig
  module Make (S: OrderedType): SET with type elt = S.t
end

(** Map constructor *)
module Map: sig
  module Make (S: OrderedType): MAP with type key = S.t
end

(** Base module, useful to abstract strings *)
module Base: sig
  type t = string
  val of_string: string -> t
  val to_string: t -> string
  module Map: MAP with type key = string
  module Set: SET with type elt = string
end

(** {2 Integer manipulation} *)

(** Map of ints *)
module IntMap: MAP with type key = int

(** Set of ints *)
module IntSet: SET with type elt = int

(** Display a list of strings *)
val string_of_list: ('a -> string) -> 'a list -> string

(** {2 String manipulation} *)

(** Map of strings *)
module StringMap: MAP with type key = string

(** Set of strings *)
module StringSet: SET with type elt = string

(** Strip a string *)
val strip: string -> string

(** Does a string starts with the given prefix ? *)
val starts_with: prefix:string -> string -> bool

(** Does a string ends with the given suffix ? *)
val ends_with: suffix:string -> string -> bool

(** Remove a prefix *)
val remove_prefix: prefix:string -> string -> string option

(** Cut a string at the first occurence of the given char *)
val cut_at: string -> char -> (string * string) option

(** Same as [cut_at], but starts from the right *)
val rcut_at: string -> char -> (string * string) option

(** Does a string contains the given chars ? *)
val contains: string -> char -> bool

(** Split a string *)
val split: string -> char -> string list

(** left indenting *)
val indent_left: string -> int -> string

(** right indenting *)
val indent_right: string -> int -> string

(** Cut a string *)
val sub_at: int -> string -> string

(** {2 Misc} *)

(** Remove from a ':' separated list of string the one with the given prefix *)
val reset_env_value: prefix:string -> string -> string list

(** if rsync -arv return 4 lines, this means that no files have changed *)
val rsync_trim: string list -> string list

(** Exact regexp matching *)
val exact_match: Re.re -> string -> bool

(** Filter and map *)
val filter_map: ('a -> 'b option) -> 'a list -> 'b list

(** Insert a value in an ordered list *)
val insert: ('a -> 'a -> int) -> 'a -> 'a list -> 'a list

(** Lazy environment *)
val getenv: string -> string

module OP: sig

  (** Pipe operator *)
  val (|>): ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

end
