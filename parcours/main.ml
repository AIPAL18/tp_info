type 'a arbre_binaire = 
    | F of 'a
    | N of 'a * 'a arbre_binaire * 'a arbre_binaire

let rec parcours_prefixe (a: 'a arbre_binaire) (f: 'a -> unit) = 
    match a with
    | F(e) -> f e
    | N(e, g, d) -> f e ; parcours_prefixe g f ; parcours_prefixe d f

let rec parcours_infixe (a: 'a arbre_binaire) (f: 'a -> unit) = 
    match a with
    | F(e) -> f e
    | N(e, g, d) -> parcours_infixe g f ; f e ; parcours_infixe d f

let rec parcours_postfixe (a: 'a arbre_binaire) (f: 'a -> unit) = 
    match a with
    | F(e) -> f e
    | N(e, g, d) -> parcours_postfixe g f ; parcours_postfixe d f ; f e

let a1 = N(1, F(2), F(4))
let a2 = N(4, F(5), F(6))
let a3 = N(6, a1, a2)
let a4 = N(7, a2, a1)

let a5 = N("A", N("B", F("D"), N("E", N("H", F("I"), F("J")), F("K"))), N("C", F("F"), F("G")))

let () = parcours_prefixe a4 (Printf.printf "%d, ") ; print_newline ()
let () = parcours_infixe a4 (Printf.printf "%d, ") ; print_newline ()
let () = parcours_postfixe a4 (Printf.printf "%d, ") ; print_newline ()

let () = parcours_prefixe a5 (Printf.printf "%s, ") ; print_newline ()
let () = parcours_infixe a5 (Printf.printf "%s, ") ; print_newline ()
let () = parcours_postfixe a5 (Printf.printf "%s, ") ; print_newline ()

