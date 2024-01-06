module type OrderedEq = sig
  type t
  val compare : t -> t -> int
  val string_of : t -> string
end
module type Numbered = sig
  val n : int
end

module type Sig = sig
  type key
  val compare : key -> key -> int
  val string_of : key -> string

  exception Not_Growing of key * key

  type 'a t = int * 'a node
  and 'a node =
    | Leaf of (key * 'a) list
    | Node of 'a lst
  and 'a lst =
    | Tail of 'a t
    | Cons of ('a t * key * 'a) * 'a lst

  val upper_limit : int
end

exception Bad_Length of int * int
exception Too_Short of int
exception Too_Long of int
exception Bad_Depth of int * int
exception InternalStructureBroken of string
exception UnexpectedHappened of string
