open Signatures

module Make(S : Sig) = struct
  open S

  let is_correct tr =
    let rec is_growing_list = function
      | (a, _) :: ((b, _) :: _ as rest) when S.compare a b < 0 ->
          is_growing_list rest
      | (a, _) :: ((b, _) :: _) ->
          raise (Not_Growing (a, b))
      | _ -> ()
    and is_growing_lst = function
      | Cons((_, a, _), (Cons((_, b, _), _) as rest)) when S.compare a b < 0 ->
          is_growing_lst rest
      | Cons((_, a, _), (Cons((_, b, _), _))) ->
          raise (Not_Growing (a, b))
      | _ -> ()
    and lst_len acc = function
      | Cons(_, rest) -> lst_len (acc+1) rest
      | Tail _ -> acc
    and correct_len = function
        | m, Leaf lst when m <> (List.length lst) ->
        raise (Bad_Length (m, (List.length lst)))
      | m, Leaf _ when m < 1 ->
        raise (Too_Short m)
      | m, Leaf _ when m >= upper_limit ->
        raise (Too_Long m)
      | m, Node lst when m <> lst_len 0 lst ->
        raise (Bad_Length (m, (lst_len 0 lst)))
      | m, Node _ when m < 1 ->
        raise (Too_Short m)
      | m, Node _ when m >= upper_limit ->
        raise (Too_Long m)
      | _ -> ()
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
          check_depth org (d+1) child;
          check_depth_lst org d rest
      | Tail child ->
          check_depth org (d+1) child
    and check_depth org d = function
      | _, Leaf _ when d = org -> ()
      | _, Leaf _ -> raise (Bad_Depth (org, d))
      | _, Node lst ->
          check_depth_lst org d lst
    and inner tr =
      correct_len tr;
      is_growing tr;
      match tr with
      | _, Node lst ->
          inner_lst lst
      | _ -> ()
    and inner_lst = function
      | Cons ((child, _, _), rest) ->
          inner child;
          inner_lst rest
      | Tail child ->
          inner child
    in
    let depth = get_depth 0 tr in
    check_depth depth 0 tr;
    inner tr;
    true

end


