open Btree_lib
open Btree_lib.Interface
open Common.Definitions


module MakeBasicTests(N : Numbered) = struct
  module IntBTree = Interface.Make(N)(IntOrderedEq)

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


