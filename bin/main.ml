open Btree_lib
open Common


let lst = [1;2;3;4;5;6;7;8;9]

let fn tr a =
  IntTree.insert a a tr

let _ =
  Logf.open_log ();
  print_endline "[";
  let tree = List.fold_left fn IntTree.empty lst in
  print_endline (IntTree.to_string string_of_int string_of_int tree ^ "]");
  IntTree.is_correct tree
