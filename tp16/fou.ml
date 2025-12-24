type 'a arbre_binaire = 
    | F of 'a
    | N of 'a * 'a arbre_binaire * 'a arbre_binaire


type 'a concat3_with = {do_concat: bool; concat: 'a list -> 'a list -> 'a list -> 'a list}

let from_option (x: 'a option) (def: 'a) = 
    match x with
    | None -> def
    | Some(x) -> x

let rec parcours_prefixe (a: 'a arbre_binaire) (c: 'b concat3_with) (on_e: 'a -> 'b) = 
    match a with
    | F(e) -> 
        if c.do_concat then 
            Some (c.concat [on_e e] [] []) 
        else
            (ignore(on_e e); None)
    | N(e, g, d) -> 
        if c.do_concat then 
            Some (c.concat [on_e e] (from_option (parcours_prefixe g c on_e) []) (from_option (parcours_prefixe d c on_e) [])) 
        else
            (ignore(on_e e); ignore(parcours_prefixe g c on_e); ignore(parcours_prefixe d c on_e); None)

let a1 = N(1, F(2), F(4))
let a2 = N(4, F(5), F(6))
let a3 = N(6, a1, a2)
let a4 = N(7, a2, a1)

let a5 = N("A", N("B", F("D"), N("E", N("H", F("I"), F("J")), F("K"))), N("C", F("F"), F("G")))

(*execute over identity*)
let eoi (f: 'a -> unit) (x: 'a) = 
    f x ; Some x


let () = ignore(parcours_prefixe a4 {do_concat = false; concat = fun x y z -> []} (eoi (Printf.printf "%d, "))); print_newline ()

let () = ignore(parcours_prefixe a4 {do_concat = true; concat = fun x y z -> (x) @ (y) @ (z)} (eoi (Printf.printf "%d, "))); print_newline ()

let rec parcours_prefixe_banal (a: 'a arbre_binaire) (f: 'a -> unit) = 
    match a with
    | F(e) -> f e
    | N(e, g, d) -> f e ; parcours_prefixe_banal g f ; parcours_prefixe_banal d f

let () = parcours_prefixe_banal a4 (Printf.printf "%d, ") ; print_newline ()