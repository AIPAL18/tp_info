open Printf

let repr_l (display: 'a -> unit) (l: 'a list)= 
  (* List.iter (fun e -> display e; print_string sep) l; print_string after *)
  let rec repr liste = 
      match liste with
      | [] -> ()
      | e::[] -> display e
      | e::q -> display e; print_string ", " ; repr q
  in
  print_string "[";
  repr l;
  print_string "]\n"

(*
-------------------------------------------------------------------------------
EXERCICE 1
-------------------------------------------------------------------------------
*)

(* QU 1:
Que renvoie List.sort (fun x y -> x - y) [7; 1; 3; 2; 0] ? 
  La liste triée dans l'ordre croissant e.i. [0; 1; 2; 3; 7]
Que renvoie List.sort (fun x y -> y - x) [7; 1; 3; 2; 0] ? 
  La liste triée dans l'ordre décroissant e.i. [7; 3; 2; 1; 0]
*)

(* tri_1 : int list -> int list *)
let tri_1 (l: int list) = 
  List.sort (fun x y -> abs x - abs y) l

let _ = repr_l (printf "%d") (tri_1 [7; 1; 3; 2; 0])
let _ = repr_l (printf "%d") (tri_1 [7; -1; 3; 2; 0])

(* QU 2:
tri_2 : (int * int) list -> (int * int) list *)

let tri_2 (l: (int * int) list) = 
  let comp ((v1_1: int), (v1_2: int)) ((v2_1: int), (v2_2: int)) =
    let c1 = abs v1_1 - abs v2_1 
    in
    if c1 = 0 then
      v2_2 - v1_2
    else c1
  in
  List.sort comp l

let _ = repr_l (fun (a, b)-> printf "(%d, %d)" a b) (tri_2 [(7, 1); (1, 2); (3, 3); (2, 4); (0, 5)])
let _ = repr_l (fun (a, b)-> printf "(%d, %d)" a b) (tri_2 [(7, 2); (-1, 2); (3, 2); (2, 2); (0, 2)])
let _ = repr_l (fun (a, b)-> printf "(%d, %d)" a b) (tri_2 [(7, 2); (-1, 2); (3, -2); (3, 1); (0, 2)])

(* tri_3 : 'a list list -> 'a list list *)

let tri_3 (l: 'a list list) =
  let comp (l1: 'a list) (l2: 'a list) =
    List.length l1 - List.length l2
  in
  List.sort comp l


let _ = repr_l (repr_l (printf "(%d)")) 
  (tri_3 [[1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]; [1; 2; 3; 4; 5]])
