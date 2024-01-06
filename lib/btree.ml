include Signatures

module Make(N : Numbered)
            (S : OrderedEq) : sig
  type key = S.t
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
  type key = S.t
  exception Not_Growing of key * key
  open Logf

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
    logf "[insert] Enter insert(%s), call insert_inner" (S.string_of k);
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

  let empty = 0, Leaf []
  let singleton k v = 1, Leaf [(k, v)]

  let of_assoc_list lst =
    List.fold_left (fun tr (k, v) -> insert k v tr) empty lst

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

  let to_string (key_to_string : key -> string) (value_to_string : 'a -> string) (tr : 'a t) = 
    let rec to_string_lst tabs = function
      | Cons((child, k, v), rest) ->
          (Printf.sprintf "{ \"child\": %s\n%s},\n%s{ \"key\": \"%s\", \"value\": \"%s\" }"
          (inner (tabs ^ "\t") child) tabs tabs (key_to_string k) (value_to_string v))
          :: (to_string_lst tabs rest)
      | Tail child ->
          [Printf.sprintf "{ \"child\": %s\n%s}" (inner (tabs ^ "\t") child) tabs]
    and to_string_list = function
      | (k, v) :: rest ->
          (Printf.sprintf "{ \"key\": \"%s\", \"value\": \"%s\" }"
          (key_to_string k) (value_to_string v)) :: (to_string_list rest)
      | [] -> []
    and string_of_list tabs = function
      | [] -> ""
      | s :: [] -> tabs ^ s ^ "\n"
      | s :: xs -> tabs ^ s ^ ",\n" ^ (string_of_list tabs xs)
    and wrap_list tabs = function
      | [] -> "[]"
      | xs -> Printf.sprintf "[\n%s%s]" (string_of_list (tabs ^ "\t") xs) tabs
    and inner tabs = function
    | m, Node lst ->
        Printf.sprintf "\n%s\"len\": \"%d\", \"lst\": %s" tabs m
        (wrap_list tabs @@ to_string_lst (tabs ^ "\t") lst)
    | m, Leaf lst ->
        Printf.sprintf "\n%s\"len\": \"%d\", \"lst\": %s" tabs m
        (wrap_list tabs @@ to_string_list lst)
    in
    (Printf.sprintf "{\n\t\"upper_limit\": \"%d\",\n\t\"content\":%s\n}" upper_limit) @@ (inner "\t" tr)

end

