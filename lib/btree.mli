module type OrderedEq = sig
  type t
  val compare : t -> t -> int
  val string_of : t -> string
end
module type Numbered = sig
  val n : int
end


module Make(N : Numbered)
            (S : OrderedEq) : sig
  type 'a t
  type key = S.t
  val upper_limit : int

  val get    : key -> 'a t -> 'a
  val insert : key -> 'a -> 'a t -> 'a t

  val empty : 'a t
  val singleton :  key -> 'a ->'a t
  val of_assoc_list : (key * 'a) list -> 'a t
  val is_correct : 'a t -> bool
  val to_string : (key -> string) -> ('a -> string) -> 'a t -> string

end
