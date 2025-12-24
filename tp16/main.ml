type 'a abr =
    | V
    | N of 'a abr * 'a * 'a abr

let rec parcours_infixe (f: 'a -> unit) (a: 'a abr)  = 
    match a with
    | V -> ()
    | N(g, e, d) -> parcours_infixe (f) g ; f e ; parcours_infixe (f) d

let rec hauteur a =
    match a with
    | V -> -1
    | N(g, _, d) -> 1 + (max (hauteur g) (hauteur d))

let exemple_3 =
N (
    N (
        N (
            N (V, -6., V),
            -5.1,
            N (V, -4.8, V)
        ),
        1.2,
        N (V, 3.1, V)
    ),
    3.5,
    N (
        N (
            N(V, 3.6, V), 
            3.8, 
            V),
        5.1,
        N (
            N(V, 5.3, V),
            10.5,
            V)
    )
)

let _ = Printf.printf "hauteur exemple_3 : %d\n---------------\n" (hauteur exemple_3)

let rec add_bot_left a b =
    (*Add a in the bottom left corner of b*)
    match b with 
    | V -> a
    | N(V, e, d) -> N(a, e, d)
    | N(g, e, d) -> N(add_bot_left a g, e, d)
    
let rec supprime a x = 
    match a with
    | V -> V
    | N(V, e, d) when e = x -> d
    | N(g, e, V) when e = x -> g
    | N(g, e, d) when e < x -> N(g, e, supprime d x)
    | N(g, e, d) when e > x -> N(supprime g x, e, d)
    | N(g, e, d)            -> add_bot_left g d


(* let _ = parcours_infixe exemple_3 (Printf.printf "%.1f, ")
let _ = print_newline ()
let a1 = supprime exemple_3 3.5
let _ = parcours_infixe a1 (Printf.printf "%.1f, ")
let _ = Printf.printf "\nhauteur: %d\n\n" (hauteur a1) *)

(* supprime_min : 'a abr -> 'a * 'a abr *)
let rec supprime_min (a: 'a abr) = 
    match a with
    | V -> failwith "ABR is empty"
    | N(V, e, d) -> e, d
    | N(g, e, d) -> let m, a' = supprime_min g in
        m, N(a', e, d)


let rec supprimer2 a x = 
    match a with
    | V -> V
    | N(V, e, d) when e = x -> d
    | N(g, e, V) when e = x -> g
    | N(g, e, d) when e < x -> N(g, e, supprime d x)
    | N(g, e, d) when e > x -> N(supprime g x, e, d)
    | N(g, e, d)            -> let m, d' = supprime_min d in 
        N(g, m, d')
    
let exec_on_tree (display: 'a -> unit) (args: 'b) (a: 'a abr) (f: 'a abr -> 'b -> 'a abr) =
    parcours_infixe display a;
    print_newline ();
    let a' = f a args in 
        parcours_infixe display a';
        Printf.printf "\nhauteur: %d\n" (hauteur a');
    print_newline ()

let _ = exec_on_tree (Printf.printf "%.1f, ") 3.5 exemple_3 supprime
let _ = exec_on_tree (Printf.printf "%.1f, ") 3.5 exemple_3 supprimer2

let _ = exec_on_tree (Printf.printf "%.1f, ") 5.1 (supprime exemple_3 3.5) supprime
let _ = exec_on_tree (Printf.printf "%.1f, ") 5.1 (supprimer2 exemple_3 3.5) supprimer2

(*
QU 5:
Non, contre exemple:
    
*)
