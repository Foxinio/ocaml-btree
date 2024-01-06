include Signatures

module Make(N : Numbered)
            (S : OrderedEq) : sig
  type key
  type 'a t

  exception Not_Growing of key * key

  val upper_limit : int

  val get    : key -> 'a t -> 'a
  val insert : key -> 'a -> 'a t -> 'a t

  val empty : 'a t
  val singleton :  key -> 'a ->'a t
  val of_assoc_list : (key * 'a) list -> 'a t
  val is_correct : 'a t -> bool
  val to_string : (key -> string) -> ('a -> string) -> 'a t -> string

end = struct

  module Types : Sig = struct 
    type key = S.t
    let compare = S.compare
    let string_of = S.string_of
    exception Not_Growing of key * key

    type 'a t = int * 'a node
    and 'a node =
      | Leaf of (key * 'a) list
      | Node of 'a lst
    and 'a lst =
      | Tail of 'a t
      | Cons of ('a t * key * 'a) * 'a lst

    let upper_limit =
      (fun n -> assert(n >= 2); n) N.n
  end

  include Types
  include Insert.Make(Types)
  include Delete.Make(Types)
  include To_string.Make(Types)
  include Is_correct.Make(Types)

  let rec get k =
    let rec get_in_lst = function
      | Tail(child) ->
        get k child
      | Cons((l, k', v), lst) ->
        let cmp = Types.compare k k' in
        if cmp = 0 then v
        else if cmp < 0 then get k l
        else get_in_lst lst
    in function
    | _, Leaf lst ->
      (List.find (fun (k', _) -> (=) 0 @@ Types.compare k k') lst)
      |> snd
    | _, Node lst ->
      get_in_lst lst

  let empty = 0, Leaf []
  let singleton k v = 1, Leaf [(k, v)]

  let of_assoc_list lst =
    List.fold_left (fun tr (k, v) -> insert k v tr) empty lst
end

