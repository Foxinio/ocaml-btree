module Btree = Btree_lib.Btree

module IntTree = Btree.Make(struct let n = 3 end)(struct
  type t = int
  let compare = compare
end)


let lst = [1;2;3;4;5;6;7;8;9]

(* let rec make_assoc_list = function *)
(*   | [] -> [] *)
(*   | x :: lst -> (x,x) :: make_assoc_list lst *)

let fn tr a =
  print_endline (IntTree.to_string string_of_int string_of_int tr ^ ",");
  IntTree.insert a a tr


let _ =
  let tree = List.fold_left fn IntTree.empty lst in
  print_endline (IntTree.to_string string_of_int string_of_int tree)
  

