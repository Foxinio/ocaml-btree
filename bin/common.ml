open Btree_lib

module IntTree = Btree.Make(struct let n = 3 end)(struct
  type t = int
  let compare = compare
  let string_of = string_of_int
end)
