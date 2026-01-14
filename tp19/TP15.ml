type 'a arbre = V | N of 'a * 'a arbre * 'a arbre

let ex =
  N
    ( 0,
      N (4, N (5, V, V), N (3, N (1, V, V), N (6, V, V))),
      N (2, N (4, V, V), V) )

let rec prefixe_inefficace a =
  match a with
  | V -> []
  | N (r, g, d) -> r :: (prefixe_inefficace g @ prefixe_inefficace d)

let rec infixe_inefficace a =
  match a with
  | V -> []
  | N (r, g, d) -> infixe_inefficace g @ (r :: infixe_inefficace d)

let rec suffixe_inefficace a =
  match a with
  | V -> []
  | N (r, g, d) -> suffixe_inefficace g @ suffixe_inefficace d @ [r]

type ('a, 'b) arbre =
  | Feuille of 'a
  | Noeud of 'b * ('a, 'b) arbre * ('a, 'b) arbre

let exemple1 =
  Noeud
    ( 12,
      Noeud
        ( 4,
          Noeud (7, Feuille true, Feuille false),
          Noeud (14, Feuille false, Feuille true) ),
      Feuille false )

let exemple2 =
  Noeud
    ( 4,
      Feuille 0.3,
      Noeud
        ( 1,
          Noeud (8, Noeud (2, Feuille 2.5, Feuille 3.1), Feuille 4.1),
          Feuille 0.2 ) )

let rec taille a =
  match a with
  | Feuille _ -> 1
  | Noeud (_, g, d) -> taille g + taille d

let rec hauteur a =
  match a with
  | Feuille _ -> 1
  | Noeud (_, g, d) ->
      let hg = hauteur g in
      let hd = hauteur d in
      if hg > hd then hg + 1 else hd + 1

let rec dernier a =
  match a with
  | Feuille v -> v
  | Noeud (_, _, d) -> dernier d

(* exercice 3 *)

type ('a, 'b) token = F of 'a | N of 'b

let rec postfixe_naif a =
  match a with
  | Feuille v -> [F v]
  | Noeud (r, g, d) -> postfixe_naif g @ postfixe_naif d @ [N r]

let postfixe a =
  let rec postfixe_aux a acc =
    match a with
    | Feuille v -> F v :: acc
    | Noeud (r, g, d) -> postfixe_aux g (postfixe_aux d (N r :: acc))
  in
  postfixe_aux a []

let prefixe a =
  let rec prefixe_aux a acc =
    match a with
    | Feuille v -> F v :: acc
    | Noeud (r, g, d) -> N r :: prefixe_aux g (prefixe_aux d acc)
  in
  prefixe_aux a []

let infixe a =
  let rec infixe_aux a acc =
    match a with
    | Feuille v -> F v :: acc
    | Noeud (r, g, d) -> infixe_aux g (N r :: infixe_aux d acc)
  in
  infixe_aux a []

(* Exercice 4 *)

let rec reconstruit_postfixe lst =
  let rec parcoure_etiquettes lst_etiquettes lst_arbres =
    match lst_etiquettes, lst_arbres with
    | [], [a] -> a
    | F v :: rest_etiquettes, _ ->
        parcoure_etiquettes rest_etiquettes (Feuille v :: lst_arbres)
    | N r :: rest_etiquettes, d :: g :: rest_arbres ->
        parcoure_etiquettes rest_etiquettes (Noeud (r, g, d) :: rest_arbres)
    | _ -> failwith "Liste d'étiquettes invalide"
  in
  parcoure_etiquettes lst []

let reconstruit_prefixe lst =
  let rec construit_premier_arbre lst_etiquettes =
    (* prend en entrée une liste d'étiquettes et tente de construire le premier
       arbre qu'elle peut avec le début de la liste, et renvoie cet arbre et le
       reste de la liste. Par exemple, si la tête est une feuille, on a
       directement gagné. Si les trois premiers éléments sont un noeud et deux
       feuilles, cela constitue aussi un arbre et on s'arrête. *)
    match lst_etiquettes with
    | F v :: rest -> Feuille v, rest
    | N r :: rest ->
        let g, rest_after_g = construit_premier_arbre rest in
        let d, rest_after_d = construit_premier_arbre rest_after_g in
        Noeud (r, g, d), rest_after_d
    | [] -> failwith "Liste d'étiquettes invalide"
  in
  match construit_premier_arbre lst with
  | a, [] -> a
  | _ -> failwith "Liste d'étiquettes invalide"

(* Exercice 6 *)

type 'a arbre_gen = N of 'a * 'a arbre_gen list

let rec prefixe_general a =
  match a with
  | N (r, lst) -> r :: List.flatten (List.map prefixe_general lst)

(* ou en un peu plus long et moins obscur : *)

let rec prefixe_general a =
  match a with
  | N (r, lst) -> r :: prefixe_foret lst

and prefixe_foret lst =
  match lst with
  | [] -> []
  | a :: rest -> prefixe_general a @ prefixe_foret rest

let rec postfixe_general a =
  match a with
  | N (r, lst) -> postfixe_foret lst @ [r]

and postfixe_foret lst =
  match lst with
  | [] -> []
  | a :: rest -> postfixe_general a @ postfixe_foret rest

let prefixe_efficace a =
  let rec prefixe_aux a acc =
    match a with
    | N (r, lst) -> r :: prefixe_foret_aux lst acc
  and prefixe_foret_aux lst acc =
    match lst with
    | [] -> acc
    | a :: rest -> prefixe_aux a (prefixe_foret_aux rest acc)
  in
  prefixe_aux a []

(* remarque : cette idée de partir d'un accumulateur, de parcourir une liste en
   mettant à jour l'accumulateur pour chaque élément est très courante en
   programmation fonctionnelle, on appelle cette opération un "fold". Ici, on
   aurait pu écrire la fonction précédente en : *)

let prefixe_efficace a =
  let rec prefixe_aux a acc =
    match a with
    | N (r, lst) ->
        r :: List.fold_right (fun x acc -> prefixe_aux x acc) lst acc
  in
  prefixe_aux a []
