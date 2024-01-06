open Btree_lib
open Btree_lib.Interface
open Common
open Common.Definitions

module MakePermTests(N : Numbered) = struct 
  module IntBTree = Interface.Make(N)(IntOrderedEq)

  let lst = [1;2;3;4;5;6;7]

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

  let string_of_list lst =
    let rec inner = function
      | [] -> ""
      | x :: [] -> string_of_int x
      | x :: xs -> string_of_int x ^ "," ^ inner xs
    in "[" ^ inner lst ^ "]"

  exception Error_on of string

  let%test "Testing all permutations of insertion" =
    let iterator l = 
      try
        let assoc_list = make_assoc_list l in
        let tree = IntBTree.of_assoc_list assoc_list in
        (List.for_all (fun (key, value) -> IntBTree.get key tree = value) assoc_list)
        && IntBTree.is_correct tree
      with
      | Too_Long n ->
          raise (Error_on ("Too_Long(" ^ string_of_int n ^ ") [m:"^ string_of_int N.n ^"]: " ^ string_of_list l))
      | InternalStructureBroken s ->
        raise (Error_on ("InternalStructureBroken(" ^ s ^ "): " ^ string_of_list l))
    in
    seq_for_all iterator @@ Perm.permutations lst
end

