open Btree_lib.Interface

module IntOrderedEq : sig
  type t = int
  val compare : t -> t -> int
  val string_of : t -> string
end

module Five  : Numbered
module Four  : Numbered
module Three : Numbered
