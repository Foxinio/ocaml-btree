open Signatures

module Make(S : Sig) : sig
 val insert : S.key -> 'a -> 'a S.t -> 'a S.t
end = struct
  open Logf
  open S

  let internal_structure_broken s = 
    raise (InternalStructureBroken (Printf.sprintf "internal structure broken: %s" s))
  let unimplemented s =
    failwith (Printf.sprintf "unimplemented: %s" s)
  let unexpected_happened s =
    raise (UnexpectedHappened (Printf.sprintf "internal structure broken: %s" s))

  let rec string_of_list = function
    | [] -> ""
    | x :: [] -> (S.string_of x)
    | x :: xs -> (S.string_of x) ^ "," ^ string_of_list xs

  let flat_string_of_lst lst =
      let rec to_list = function
      | Cons((_, k, _), rest) ->
        k :: to_list rest
      | Tail _ -> []
      in
      string_of_list @@ to_list lst
  let flat_string_of_list lst =
      let rec to_list = function
      | (k, _) :: rest ->
        k :: to_list rest
      | [] -> []
      in
      string_of_list @@ to_list lst

  let rec assoc_insert1 k v = function
    | (k', _)  :: rest when S.compare k k' = 0 ->
        (k, v) :: rest
    | (k', v') :: rest when S.compare k k' < 0 ->
        (k, v) :: (k', v') :: rest
    | p :: rest -> p :: (assoc_insert1 k v rest)
    | [] -> [(k, v)]

  let split = function
    | Node(Cons((child1, k1, v1), Cons((child2, k2, v2), rest))) ->
        logf "[split] Enter split on k: %s" (S.string_of k1);
        let inner_left = Node(Cons((child1, k1, v1), Tail child2)) in 
        logf "[split] Returning Node {(%s)/(%s)\\(%s)}" (S.string_of k1)
        (S.string_of k2) (flat_string_of_lst rest);
        ((1, inner_left), k2, v2), Node rest
    | Leaf ((k1, v1) :: (k2, v2) :: rest) ->
        logf "[split] Enter split on k: %s" (S.string_of k1);
        let inner_left = Leaf [(k1, v1)] in
        logf "[split] Returning Leaf {(%s)/(%s)\\(%s)}" (S.string_of k1)
        (S.string_of k2) (flat_string_of_list rest);
        ((1, inner_left), k2, v2), Leaf rest
    | _structure -> internal_structure_broken "tried to split tier-one or tier-two node/leaf"

  let rec insert_inner k v tr = 
    match tr with
    | m, Leaf lst ->
        logf "[insert_inner] Enter insert_inner, inserting into Leaf, m: %d" m;
        let assoc = assoc_insert1 k v lst in 
        logf "[insert_inner] Exiting insert_inner, assoc_insert1 returned: [%s], m: %d"
        (flat_string_of_list assoc) (m+1);
        m+1, Leaf assoc
    | m, Node lst -> 
        logf "[insert_inner] Enter insert_inner, inserting into Node, m: %d" m;
        let m, updated = assoc_insert2 m k v lst in
        logf "[insert_inner] Exiting insert_inner, assoc_insert2 returned: [%s], m: %d"
        (flat_string_of_lst updated) m;
        m, Node updated

  and assoc_insert2 m k v = function
    | Cons ((child, k', _), rest) when S.compare k k' = 0 ->
      logf "[assoc_insert2] Enter assoc_insert2, [%s=%s]" (S.string_of k) (S.string_of k');
      m, Cons ((child, k, v), rest)
    | Cons ((child, k', v'), rest) when S.compare k k' < 0 ->
      logf "[assoc_insert2] Enter assoc_insert2, [%s<%s]" (S.string_of k) (S.string_of k');
      let m', updated = insert_inner k v child in
      if m' < upper_limit then
        (logf "[assoc_insert2] Exiting, [m: %d < upper_limit: %d]" m upper_limit;
        m, Cons(((m', updated), k', v'), rest))
      else
        (logf "[assoc_insert2] Upper limit reached, calling split [upper_limit: %d]" upper_limit;
        let left, right = split updated in
        let updated = Cons(((m' - 2, right), k', v'), rest) in
        m+1, Cons(left, updated))
    | Cons((_, k', _) as p, Tail(child)) ->
      logf "[assoc_insert2] Enter assoc_insert2, [%s>%s]" (S.string_of k) (S.string_of k');
      let m', updated = insert_inner k v child in
      if m' < upper_limit then
        (logf "[assoc_insert2] Exiting, [m: %d < upper_limit: %d]" m upper_limit;
        m, Cons(p, Tail(m', updated)))
      else
        (logf "[assoc_insert2] Upper limit reached, calling split [upper_limit: %d]" upper_limit;
        let left, right = split updated in
        let updated = Cons(left, Tail(m' - 2, right)) in
        m+1, Cons(p, updated))
    | Cons (p, rest) ->
      logf "[assoc_insert2] calling assoc_insert2 recursively";
      let m', res = assoc_insert2 m k v rest in
      m', Cons(p, res)
    | Tail child ->
        logf "[assoc_insert2] Reached unexpected state: assoc has been called on tier-zero node";
        m, Tail (insert k v child)
        (* internal_structure_broken "called assoc on tier-zero node" *)

  and insert k v tr =
    logf "-----------------------------------------------";
    logf "[insert] Enter insert(%s), call insert_inner" (string_of k);
    let m, updated = insert_inner k v tr in
    logf "[insert] Returned from insert_inner with m: %d" m;
    if m < upper_limit then
      (logf "[insert] Exiting insert";
      logf "-----------------------------------------------";
      m, updated)
    else 
      (let left, right = split updated in
        logf "[insert] Exiting insert";
        logf "-----------------------------------------------";
        1, Node(Cons(left, Tail (m - 2,right))))

end

