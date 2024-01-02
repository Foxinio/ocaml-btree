module type OrderedEq = sig
  type t
  val compare : t -> t -> int
end
module type Numbered = sig
  val n : int
end


exception InternalStructureBroken of string
exception UnexpectedHappened of string

module Make(N : Numbered)
            (S : OrderedEq) : sig
  type key = S.t
  type 'a t

  val upper_limit : int

  val get    : key -> 'a t -> 'a
  val insert : key -> 'a -> 'a t -> 'a t
  val delete : key -> 'a t -> 'a t

  val empty : 'a t
  val singleton :  key -> 'a ->'a t
  val of_assoc_list : (key * 'a) list -> 'a t
  val is_correct : 'a t -> bool
  val to_string : (key -> string) -> ('a -> string) -> 'a t -> string

end = struct
  type key = S.t

  type 'a t = int * 'a node
  and 'a node =
    | Leaf of (key * 'a) list
    | Node of 'a lst
  and 'a lst =
    | Tail of 'a t
    | Cons of ('a t * key * 'a) * 'a lst

  let upper_limit =
    (fun n -> assert(n >= 2); n) N.n


  let rec get k =
    let rec get_in_lst = function
      | Tail(child) ->
        get k child
      | Cons((l, k', v), lst) ->
        let cmp = S.compare k k' in
        if cmp = 0 then v
        else if cmp < 0 then get k l
        else get_in_lst lst
    in function
    | _, Leaf lst ->
      (List.find (fun (k', _) -> (=) 0 @@ S.compare k k') lst)
      |> snd
    | _, Node lst ->
      get_in_lst lst

  let internal_structure_broken s = 
    raise (InternalStructureBroken (Printf.sprintf "internal structure broken: %s" s))
  let unimplemented s =
    failwith (Printf.sprintf "unimplemented: %s" s)
  let unexpected_happened s =
    raise (UnexpectedHappened (Printf.sprintf "internal structure broken: %s" s))

  let rec assoc_insert1 k v = function
    | (k', _)  :: rest when S.compare k k' = 0 ->
        (k, v) :: rest
    | (k', v') :: rest when S.compare k k' < 0 ->
        (k, v) :: (k', v') :: rest
    | p :: rest -> p :: (assoc_insert1 k v rest)
    | [] -> [(k, v)]

  let split = function
    | Node(Cons((child1, k1, v1), Cons((child2, k2, v2), rest))) ->
        let inner_left = Node(Cons((child1, k1, v1), Tail child2)) in 
        ((1, inner_left), k2, v2), Node rest
    | Leaf ((k1, v1) :: (k2, v2) :: rest) ->
        let inner_left = Leaf [(k1, v1)] in
        ((1, inner_left), k2, v2), Leaf rest
    | _structure -> internal_structure_broken "tried to split tier-one or tier-two node/leaf"

  let rec insert k v tr = 
    match tr with
    | m, Leaf lst -> m+1, Leaf (assoc_insert1 k v lst)
    | m, Node lst -> 
        let m, updated = assoc_insert2 m k v lst in
        m, Node updated

  and assoc_insert2 m k v = function
    | Cons ((child, k', _), rest) when S.compare k k' = 0 ->
        m, Cons ((child, k, v), rest)
    | Cons ((child, k', v'), rest) when S.compare k k' < 0 ->
      let m', updated = insert k v child in
      if m' < upper_limit then m, Cons(((m', updated), k', v'), rest)
      else
        let left, right = split updated in
        let updated = Cons(((m' - 2, right), k', v'), rest) in
        m+1, Cons(left, updated)
    | Cons(p, Tail(child)) ->
      let m', updated = insert k v child in
      if m' < upper_limit then m, Tail(m', updated)
      else
        let left, right = split updated in
        let updated = Cons(left, Tail(m' - 2, right)) in
        m+1, Cons(p, updated)
    | Cons (p, rest) ->
      let m', res = assoc_insert2 m k v rest in
      m', Cons(p, res)
    | Tail _ -> internal_structure_broken "called assoc on tier-zero node"

  let get_left_sibling =
    let rec get_left_list = function
      | _ :: [] as rest -> None, rest
      | p1 :: p2 :: [] -> Some(p1, Leaf []), [p2]
      | p :: (_ :: _) as rest ->
          let p2, rest = get_left_list rest in
          p2, p :: rest
      | [] -> None, []
    and get_left_lst = function
      | Cons(_, Tail _) as cons ->
          None, cons
      | Cons(p, Cons((child, k, v), Tail rest)) ->
          Some((k,v),Node(Tail rest)), Cons(p, Tail child)
      | Cons(p, (Cons _ as lst)) ->
          let last, lst = get_left_lst lst in
          last, Cons(p, lst)
      | Tail _ -> internal_structure_broken "called get_left_sibling on tier-zero node" in
    function
    | m, Leaf lst ->
        let last, lst = get_left_list lst in
        last, (m-1, Leaf lst)
    | m, Node lst ->
        let last, lst = get_left_lst lst in
        last, (m-1, Node lst)

  let get_right_sibling = 
    let get_sibling = function
    | m, Leaf (p1 :: p2 :: rest) ->
        Some(p1, Leaf []), (m-1, Leaf (p2 :: rest))
    | m, Leaf [p] ->
        None, (m, Leaf [p])
    | m, Node (Cons((child, k, v), Cons(p, rest))) ->
        Some((k,v), Node(Tail child)), (m-1, Node (Cons(p, rest)))
    | m, (Node (Cons (_, Tail _)) as node) ->
        None, (m, node)
    | _, Leaf []
    | _, Node (Tail _) ->
        internal_structure_broken "called get_right_sibling on tier-zero node/leaf" in
    function
    | Tail child ->
        let min, rest = get_sibling child in
        min, Tail rest
    | Cons ((child, k80, v), rest90) ->
        let min, _60 = get_sibling child in
        min, Cons((_60, k80, v), rest90)

  let delete = 
    let rec assoc_delete1 k = function
      | (k', _) :: rest when S.compare k k' = 0 ->
          rest
      | p :: rest ->
          p :: (assoc_delete1 k rest)
      | [] ->
          raise Not_found
    and assoc_delete2 m1 k1 = function
      | Cons((_child, k2, _), _lst) when S.compare k1 k2 = 0 ->
          unimplemented "In node case"
      | Cons(p, Cons((child, k2, v2), lst)) when S.compare k1 k2 < 0 ->
          let m2, updated = delete_inner k1 child in
          if m2 > 0 then m1, Cons(p, Cons(((m2, updated), k2, v2), lst))
          else
            fix_broken_too_short m1 p (updated, k2, v2) lst
      | Cons(p, Tail child) ->
          let m2, updated = delete_inner k1 child in
          if m2 > 0 then m1, Cons(p, Tail (m2, updated))
          else
            unimplemented "too short node, from the right"
      (* this happens only when deleting from the left of tier-one node *)
      | Cons((child, k2, v2), lst) when S.compare k1 k2 < 0 ->
          let m2, updated = delete_inner k1 child in
          if m2 > 0 then m1, Cons(((m2, updated), k2, v2), lst)
          else
            unimplemented "too short node, from the left, tier-one node"
      | Cons(p, rest) ->
          let m2, updated = assoc_delete2 m1 k1 rest in
          m2, Cons(p, updated)
      | Tail _ -> internal_structure_broken "called assoc_delete2 on tier-zero node"
    and delete_inner (k : key) : 'a t -> 'a t = function
      | m, Leaf lst -> m-1, Leaf (assoc_delete1 k lst)
      | m, Node lst ->
          let m, updated = assoc_delete2 m k lst in
          m, Node updated
    and fix_broken_too_short m1 (child_l, k3, v3) (updated, k2, v2) lst =
      match get_left_sibling child_l, updated with
      | (Some((k4, v4), Leaf _), subst), Leaf _ ->
          let inner_right = Cons(((1, Leaf [(k3, v3)]), k2, v2), lst) in
          m1, Cons ((subst, k4, v4), inner_right)
      | (Some((k4, v4), Node(Tail child)), subst), Node(Tail child2) ->
          let inner_right = 1, Node(Cons((child, k3, v3), Tail child2)) in
          let upper_right = Cons((inner_right, k2, v2), lst) in
          m1, Cons ((subst, k4, v4), upper_right)
      | (Some _, _), _p ->
          unexpected_happened "get_left_sibling returned some, but updated didn't match"
      | (None, _), _ -> 
          begin match get_right_sibling lst, updated with
          | (Some((k4, v4), Leaf []), subst), Leaf _ ->
              let inner_left = Cons(((1, Leaf [(k2, v2)]), k4, v4), subst) in
              m1, Cons((child_l, k3, v3), inner_left)
          | (Some((k4, v4), Node(Tail left_50)), subst), Node(Tail right40) ->
              let inner_left = 1, Node(Cons((right40, k2, v2), Tail left_50)) in
              let upper_left = Cons((inner_left, k4, v4), subst) in
              m1, Cons((child_l, k3, v3), upper_left)
          | (Some _, _), _p ->
              unexpected_happened "get_left_sibling returned some, but updated didn't match"
          | (None, _), _ ->
              unimplemented "merging down"
          end


    in
    delete_inner


  let empty = 0, Leaf []
  let singleton k v = 1, Leaf [(k, v)]

  let of_assoc_list lst =
    List.fold_left (fun tr (k, v) -> insert k v tr) empty lst

  exception Not_Growing of key * key
  exception Bad_Length of int * int
  exception Too_Short of int
  (* exception Too_Long of int *)
  exception Bad_Depth of int * int

  let is_correct tr =
    let rec is_growing_list = function
      | (a, _) :: ((b, _) :: _) as rest when S.compare a b < 0 ->
          is_growing_list rest
      | (a, _) :: ((b, _) :: _) ->
          raise (Not_Growing (a, b))
      | _ -> true
    and is_growing_lst = function
      | Cons((_, a, _), (Cons((_, b, _), _) as rest)) when S.compare a b < 0 ->
          is_growing_lst rest
      | Cons((_, a, _), (Cons((_, b, _), _))) ->
          raise (Not_Growing (a, b))
      | _ -> true
    and lst_len acc = function
      | Cons(_, rest) -> lst_len (acc+1) rest
      | Tail _ -> acc
    and correct_len = function
      | m, Leaf lst when m <> (List.length lst) ->
        raise (Bad_Length (m, (List.length lst)))
      | m, Leaf _ when m < 1 ->
        raise (Too_Short m)
      (* | m, Leaf _ when m >= upper_limit -> *)
      (*   raise (Too_Long m) *)
      | m, Node lst when m = lst_len 0 lst ->
        raise (Bad_Length (m, (lst_len 0 lst)))
      | m, Node _ when m >= 1 ->
        raise (Too_Short m)
      (* | m, Node _ when m < upper_limit -> *)
      (*   raise (Too_Long m) *)
      | _ -> true
    and is_growing = function
      | _, Leaf lst -> is_growing_list lst
      | _, Node lst -> is_growing_lst lst
    and get_depth acc = function
      | _, Leaf _ -> acc
      | _, Node (Cons((child, _, _), _)) ->
          get_depth (acc+1) child
      | _, Node (Tail child) ->
          get_depth (acc+1) child
    and check_depth_lst org d = function
      | Cons ((child, _, _), rest) ->
          check_depth org (d+1) child && check_depth_lst org d rest
      | Tail child ->
          check_depth org (d+1) child
    and check_depth org d = function
      | _, Leaf _ when d = org -> true
      | _, Leaf _ -> raise (Bad_Depth (org, d))
      | _, Node lst ->
          check_depth_lst org d lst
    and inner tr =
      correct_len tr && is_growing tr && 
      match tr with
      | _, Node lst ->
          inner_lst lst
      | _ -> true
    and inner_lst = function
      | Cons ((child, _, _), rest) ->
          inner child && inner_lst rest
      | Tail child ->
          inner child
    in
    let depth = get_depth 0 tr in
    check_depth depth 0 tr && inner tr

  let to_string key_to_string value_to_string = 
    let rec to_string_lst = function
      | Cons((child, k, v), rest) ->
          Printf.sprintf "\n{ \"child\": %s, \"key\": \"%s\", \"value\": \"%s\" },%s"
          (inner child) (key_to_string k) (value_to_string v) (to_string_lst rest)
      | Tail child ->
          Printf.sprintf "{ \"child\": %s }" (inner child)
    and to_string_list = function
      | (k, v) :: rest ->
          Printf.sprintf "\n{ \"key\": \"%s\", \"value\": \"%s\" },%s"
          (key_to_string k) (value_to_string v) (to_string_list rest)
      | [] -> ""
    and inner = function
    | m, Node lst ->
        Printf.sprintf "\n{ \"len\": \"%d\", \"lst\": [ %s\n] }" m (to_string_lst lst)
    | m, Leaf lst ->
        Printf.sprintf "\n{ \"len\": \"%d\", \"lst\": [ %s\n] }" m (to_string_list lst)
    in inner

end

