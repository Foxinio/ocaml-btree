open Btree_lib
open Btree_lib.Interface
open Common


let lst = [1;2;3;4;5;6]

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

let enumerate seq =
  let rec inner index seq () =
    match seq () with
    | Seq.Nil -> Seq.empty ()
    | Seq.Cons (elem, rest) ->
        Seq.Cons ((index, elem), inner (index + 1) rest)
  in
  inner 0 seq

let _ =
  let iterator (i,l) = 
    Printf.printf "[i:%d]: %s\n%!" i (string_of_list l);
    let assoc_list = make_assoc_list l in
    let tree = IntTree.of_assoc_list assoc_list in
    let handle = open_out ("out/tree_m"^string_of_int 3 ^"_"^string_of_list l^".out") in
    Printf.fprintf handle "%s" (IntTree.to_string string_of_int string_of_int tree);
    close_out_noerr handle;
    try
      (List.for_all (fun (key, value) -> IntTree.get key tree = value) assoc_list)
      && IntTree.is_correct tree
    with
    | Too_Long n ->
        raise (Error_on ("Too_Long(" ^ string_of_int n ^ ") [m:"^ string_of_int 3 ^"]: " ^ string_of_list l))
    | InternalStructureBroken s ->
      raise (Error_on ("InternalStructureBroken(" ^ s ^ "): " ^ string_of_list l))
  in
  seq_for_all iterator @@ enumerate @@ Perm.permutations lst


