let insert_at_all_positions x lst =
  let rec aux prefix suffix acc =
    match suffix with
    | [] -> List.rev_append acc [x :: prefix]
    | hd :: tl -> aux (hd :: prefix) tl ((List.rev_append prefix (x :: hd :: tl)) :: acc)
  in
  aux [] lst []

let rec permutations lst =
  match lst with
  | [] -> Seq.return []
  | x :: tl ->
    Seq.flat_map (fun perm -> List.to_seq (insert_at_all_positions x perm)) (permutations tl)
