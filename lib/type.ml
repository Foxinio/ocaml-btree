(* open Signatures *)
(* *)
(* module type Sig = sig *)
(*   type key *)
(*   exception Not_Growing of key * key *)
(* *)
(*   type 'a t = int * 'a node *)
(*   and 'a node = *)
(*     | Leaf of (key * 'a) list *)
(*     | Node of 'a lst *)
(*   and 'a lst = *)
(*     | Tail of 'a t *)
(*     | Cons of ('a t * key * 'a) * 'a lst *)
(* *)
(*   val upper_limit : int *)
(* end *)

(* module Make(N : Numbered) *)
(*             (S : OrderedEq) : sig *)
(*   type key = S.t *)
(*   exception Not_Growing of key * key *)
(* *)
(*   type 'a t = int * 'a node *)
(*   and 'a node = *)
(*     | Leaf of (key * 'a) list *)
(*     | Node of 'a lst *)
(*   and 'a lst = *)
(*     | Tail of 'a t *)
(*     | Cons of ('a t * key * 'a) * 'a lst *)
(* *)
(*   val upper_limit : int *)

  (* let internal_structure_broken s =  *)
  (*   raise (InternalStructureBroken (Printf.sprintf "internal structure broken: %s" s)) *)
  (* let unimplemented s = *)
  (*   failwith (Printf.sprintf "unimplemented: %s" s) *)
  (* let unexpected_happened s = *)
  (*   raise (UnexpectedHappened (Printf.sprintf "internal structure broken: %s" s)) *)
(* end = struct *)
(*   type key = S.t *)
(*   exception Not_Growing of key * key *)
(* *)
(*   type 'a t = int * 'a node *)
(*   and 'a node = *)
(*     | Leaf of (key * 'a) list *)
(*     | Node of 'a lst *)
(*   and 'a lst = *)
(*     | Tail of 'a t *)
(*     | Cons of ('a t * key * 'a) * 'a lst *)
(* *)
(*   let upper_limit = *)
(*     (fun n -> assert(n >= 2); n) N.n *)
(* end *)
