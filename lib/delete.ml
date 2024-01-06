open Signatures

module Make(S : Sig) : sig
  val delete : S.key -> 'a S.t -> 'a S.t
end = struct
  open Logf
  open S

  let internal_structure_broken s = 
    raise (InternalStructureBroken (Printf.sprintf "internal structure broken: %s" s))
  let unimplemented s =
    failwith (Printf.sprintf "unimplemented: %s" s)
  let unexpected_happened s =
    raise (UnexpectedHappened (Printf.sprintf "internal structure broken: %s" s))

  let get_left_sibling =
    let rec get_left_list = function
      | _ :: [] as rest -> None, rest
      | p1 :: p2 :: [] -> Some(p1, Leaf []), [p2]
      | p :: rest ->
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

  let delete k tr = 
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
    logf "-----------------------------------------------";
    logf "[delete] Enter delete(%s), call delete_inner" (S.string_of k);
    let res = delete_inner k tr in
    logf "[delete] Exiting delete";
    logf "-----------------------------------------------";
    res


end

