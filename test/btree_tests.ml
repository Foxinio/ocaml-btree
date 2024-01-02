open Btree_lib
open Btree_lib.Btree

module IntOrderedEq = struct
  type t = int
  let compare a b = compare a b
  let string_of = string_of_int
end

module MakeBasicTests(N : Numbered) = struct

  module IntBTree = Btree.Make(N)(IntOrderedEq)

  let%test "Empty tree should raise Not_found when searching for a key" =
    try
      let _ = IntBTree.get 42 IntBTree.empty in
      false
    with Not_found -> true

  let%test "Inserting and retrieving a single element should work" =
    let tree = IntBTree.insert 42 "Hello" IntBTree.empty in
    IntBTree.get 42 tree = "Hello"

  let%test "Inserting multiple elements and retrieving them should work" =
    let tree = IntBTree.insert 42 "Hello" (IntBTree.insert 24 "World" IntBTree.empty) in
    IntBTree.get 42 tree = "Hello" && IntBTree.get 24 tree = "World"

  let%test "Inserting and updating an element should work" =
    let tree = IntBTree.insert 42 "Hello" IntBTree.empty in
    let updated_tree = IntBTree.insert 42 "Updated" tree in
    IntBTree.get 42 updated_tree = "Updated"

  let%test "Creating a singleton tree and retrieving the element should work" =
    let tree = IntBTree.singleton 42 "Hello" in
    IntBTree.get 42 tree = "Hello"

  let%test "Creating a tree from an association list and retrieving elements should work" =
    let assoc_list = [(42, "Hello"); (24, "World"); (10, "BTree")] in
    let tree = IntBTree.of_assoc_list assoc_list in
    List.for_all (fun (key, value) -> IntBTree.get key tree = value) assoc_list

end

module M2Basic = MakeBasicTests(struct
  let n = 2
end)

module M3Basic = MakeBasicTests(struct
  let n = 3
end)

module M4Basic = MakeBasicTests(struct
  let n = 4
end)

module M5Basic = MakeBasicTests(struct
  let n = 5
end)

module MakePermTests(N : Numbered) = struct 

  module IntBTree = Btree.Make(N)(IntOrderedEq)

  let lst = [1;2;3;4;5]

  let rec make_assoc_list = function
    | [] -> []
    | x :: lst -> (x,x) :: make_assoc_list lst

  let seq_for_all pred seq =
    let rec check_all pred seq =
      match seq () with
      | Seq.Nil -> true
      | Seq.Cons (elem, rest) when pred elem ->
          check_all pred rest
      | Seq.Cons _ -> false
    in
    check_all pred seq

  let%test "Testing all permutations of insertion" =
    let iterator lst = 
      let assoc_list = make_assoc_list lst in
      let tree = IntBTree.of_assoc_list assoc_list in
      (List.for_all (fun (key, value) -> IntBTree.get key tree = value) assoc_list)
      && IntBTree.is_correct tree
    in
    seq_for_all iterator @@ Perm.permutations lst

end

module M2Perm = MakePermTests(struct
  let n = 2
end)

module M3Perm = MakePermTests(struct
  let n = 3
end)

module M4Perm = MakePermTests(struct
  let n = 4
end)

module M5Perm = MakePermTests(struct
  let n = 5
end)
