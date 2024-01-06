open Signatures

module Make(S : Sig) = struct
  open S

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

