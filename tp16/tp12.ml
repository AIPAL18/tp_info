(* Exercice 1 *)

type 'a arbre =
    | V
    | N of 'a * 'a arbre * 'a arbre

let rec somme_etiquettes arb =
    match arb with
    | V -> 0
    | N(v, g, d) -> v + somme_etiquettes g + somme_etiquettes d

let () = Printf.printf "%d\n" (somme_etiquettes (N(1, N(2, V, V), N(2, V, V))))

let rec max_etiquettes arb = 
    match arb with
    | V -> failwith "wrong"
    | N(e, V, V) -> e
    | N(e, g, d) -> max e (max (max_etiquettes g) (max_etiquettes d))

let () = Printf.printf "%d\n" (max_etiquettes (N(100, N(2, V, V), N(10, V, V))))
let () = Printf.printf "%d\n" (max_etiquettes (N(1, N(2, V, V), N(10, V, V))))

let rec somme_feuilles arb =
    match arb with 
    | V -> failwith "wrong"
    | N(e, V, V) -> e
    | N(_, V, a) | N(_, a, V) -> somme_feuilles a
    | N(_, d, g) -> somme_feuilles d + somme_feuilles g

let () = Printf.printf "%d\n" (somme_feuilles (N(1, N(2, V, V), N(10, V, V))))

type 'a arbre_strict =
    | Feuille of 'a
    | Noeud_Interne of 'a arbre_strict * 'a * 'a arbre_strict

let rec hauteur_str (arb: 'a arbre_strict) =
    match arb with
    | Feuille(_) -> 1
    | Noeud_Interne(g, _, d) -> 1 + (max (hauteur_str g) (hauteur_str d))

let () = Printf.printf "%d\n" (hauteur_str (
    Noeud_Interne(Noeud_Interne(Feuille(1), 1, Feuille(1)), 1, Noeud_Interne(Feuille(1), 1, Feuille(1)))
))
let () = Printf.printf "%d\n" (hauteur_str (Feuille(1)))

let rec nb_feuilles (arb: 'a arbre_strict) =
    match arb with
    | Feuille(_) -> 1
    | Noeud_Interne(g, _, d) -> nb_feuilles g + nb_feuilles d

let () = Printf.printf "%d\n" (nb_feuilles (
    Noeud_Interne(Noeud_Interne(Feuille(1), 1, Feuille(1)), 1, Noeud_Interne(Feuille(1), 1, Feuille(1)))
))
let () = Printf.printf "%d\n" (nb_feuilles (Feuille(1)))

let rec max_etiquettes (arb: 'a arbre_strict) =
    match arb with
    | Feuille(e) -> e
    | Noeud_Interne(g, e, d) -> max e (max (max_etiquettes g) (max_etiquettes d))

let () = Printf.printf "%d\n" (max_etiquettes (
    Noeud_Interne(Noeud_Interne(Feuille(1), 10, Feuille(1)), 1, Noeud_Interne(Feuille(1), 1, Feuille(1)))
))
let () = Printf.printf "%d\n" (max_etiquettes (Feuille(1100)))
        
