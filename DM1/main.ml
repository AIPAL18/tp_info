open Printf
(* Si enabled vaut true, les assertion seront exécutées *)
let (enabled: bool) = true

(* Types *)
type trie = V | N of char * trie * trie
type mot = char list

(* Exemples *)
let exemple = ["sire";"site";"ski";"sac";"dodos";"dodu";"dole";"de";"si";"do";]
let exemple2 = ["barbue"; "barbier"; "barbie"; "banquier"; "banque"; "banquet"; "braquage"; "barbecue"; "assiette"; "assise"; "avion"; "arriere"; "arbitre"; "abris";]
let trie_exemple = N ( 'd', N ( 'o', N ( '$', V, N ( 'l', N ('e', N ('$', V, V), V), N ( 'd', N ( 'u', N ('$', V, V), N ('o', N ('s', N ('$', V, V), V), V) ), V ) ) ), N ('e', N ('$', V, V), V) ), N ( 's', N ( 'i', N ( '$', V, N ( 't', N ('e', N ('$', V, V), V), N ('r', N ('e', N ('$', V, V), V), V) ) ), N ( 'a', N ('c', N ('$', V, V), V), N ('k', N ('i', N ('$', V, V), V), V) ) ), V ) )
let trie_exemple2 = N ('b', N ('a', N ('r', N ('b', N ('u', N ('e', N ('$', V, V), V), N ('i', N ('e', N ('r', N ('$', V, V), N ('$', V, V)), V), N ('e', N ('c', N ('u', N ('e', N ('$', V, V), V), V), V), V))), V), N ('n', N ('q', N ('u', N ('i', N ('e', N ('r', N ('$', V, V), V), V), N ('e', N ('$', V, N ('t', N ('$', V, V), V)), V)), V), V), V)), N ('r', N ('a', N ('q', N ('u', N ('a', N ('g', N ('e', N ('$', V, V), V), V), V), V), V), V), V)), N ('a', N ('s', N ('s', N ('i', N ('e', N ('t', N ('t', N ('e', N ('$', V, V), V), V), V), N ('s', N ('e', N ('$', V, V), V), V)), V), V), N ('v', N ('i', N ('o', N ('n', N ('$', V, V), V), V), V), N ('r', N ('r', N ('i', N ('e', N ('r', N ('e', N ('$', V, V), V), V), V), V), N ('b', N ('i', N ('t', N ('r', N ('e', N ('$', V, V), V), V), V), V), V)), N ('b', N ('r', N ('i', N ('s', N ('$', V, V), V), V), V), V)))), V))

(* Comparaisons et égalités *)

let eq_mot (mot1: mot) (mot2: mot) = 
  if List.length mot1 != List.length mot2 then false
  else
    let rec inner_comp m1 m2 = 
      match m1, m2 with
      | [], [] -> true
      | t1::q1, t2::q2 when t1 = t2 -> inner_comp q1 q2
      | _ -> false
    in inner_comp mot1 mot2
  
let rec eq_mot_list (ml1: mot list) (ml2: mot list) = 
  if List.length ml1 != List.length ml2 then false
  else
    match ml1 with
    | [] -> true
    | t::q -> (is_in t ml2) && (eq_mot_list q (remove t ml2))
and remove (e: 'a) (l: 'a list) =
  match l with
  | [] -> l
  | t::q when t = e -> q
  | t::q -> t::remove e q
and is_in (e: 'a) (l: 'a list) = 
  match l with
  | [] -> false
  | t::q when eq_mot t e -> true
  | t::q -> is_in e q

(*
Compare `f arg` et `expected` en utilisant `comp`.

Pour des raisons de variadicité (variabilité de l'arité), comp prend deux entrées
de types différents.
On effectue une comparaison qui peut être une égalité, mais non nécessairement.
Dans le cas échéant, l'égalité sera sous-jacente.
*)
let comp_partial (comp: 'b -> 'c -> bool) (f: 'a -> 'b) ((arg: 'a), (expected: 'c)) =
  comp (f arg) expected

(* Samples *)
  
let est_bien_forme_tests = [
  (N('a', N('$', N('a',  N('$', V, V), V), V), V), false);
  (N('a', V, N('b', N('a', V, V), N('b', V, V))), false);
  (N('a', V, N('b', N('a', V, V), N('b', V, V))), false);
  (V, true);
  (N('a', N('$', V, N('c', N('$', V, V), V)), N('b', N('$', V, V), V)), true);
]

let mot_of_string_tests = [
  ("banane", ['b'; 'a'; 'n'; 'a'; 'n'; 'e'; '$']);
  ("coucou", ['c';'o';'u';'c';'o';'u';'$']);
  ("bonjour", ['b'; 'o'; 'n'; 'j'; 'o'; 'u'; 'r'; '$']);
  ("c'est génial", ['c'; '\''; 'e'; 's'; 't'; ' '; 'g'; '\195'; '\169'; 'n'; 'i'; 'a'; 'l'; '$']);
  ("", ['$'])
]

let mots_of_trie_tests = [
  (trie_exemple, [['d'; 'o'; '$']; ['d'; 'o'; 'l'; 'e'; '$']; ['d'; 'o'; 'd'; 'u'; '$'];['d'; 'o'; 'd'; 'o'; 's'; '$']; ['d'; 'e'; '$']; ['s'; 'i'; '$'];['s'; 'i'; 't'; 'e'; '$']; ['s'; 'i'; 'r'; 'e'; '$']; ['s'; 'a'; 'c'; '$'];['s'; 'k'; 'i'; '$']]);
  (trie_exemple2, [['b'; 'a'; 'r'; 'b'; 'u'; 'e'; '$']; ['b'; 'a'; 'r'; 'b'; 'i'; 'e'; 'r'; '$']; ['b'; 'a'; 'r'; 'b'; 'i'; 'e'; '$']; ['b'; 'a'; 'r'; 'b'; 'e'; 'c'; 'u'; 'e'; '$']; ['b'; 'a'; 'n'; 'q'; 'u'; 'i'; 'e'; 'r'; '$']; ['b'; 'a'; 'n'; 'q'; 'u'; 'e'; '$']; ['b'; 'a'; 'n'; 'q'; 'u'; 'e'; 't'; '$']; ['b'; 'r'; 'a'; 'q'; 'u'; 'a'; 'g'; 'e'; '$']; ['a'; 's'; 's'; 'i'; 'e'; 't'; 't'; 'e'; '$']; ['a'; 's'; 's'; 'i'; 's'; 'e'; '$']; ['a'; 'v'; 'i'; 'o'; 'n'; '$']; ['a'; 'r'; 'r'; 'i'; 'e'; 'r'; 'e'; '$']; ['a'; 'r'; 'b'; 'i'; 't'; 'r'; 'e'; '$']; ['a'; 'b'; 'r'; 'i'; 's'; '$']]);
  (N('a', N('$', V, N('c', N('$', V, V), V)), N('b', N('$', V, V), V)), [['a'; '$']; ['a'; 'c'; '$']; ['b'; '$'];]);
]

let cardinal_tests = [
  (trie_exemple, 10);
  (trie_exemple2, 14);
  (N('a', N('$', V, N('c', N('$', V, V), V)), N('b', N('$', V, V), V)), 3);
]

let recherche_tests = [
  (trie_exemple, (['s'; 'i'; 't'; 'e'; '$'], true));
  (trie_exemple, (['s'; 'a'; 'c'; '$'], true));
  (N('a', N('$', V, N('c', N('$', V, V), V)), N('b', N('$', V, V), V)), (['a'; '$'], true));
  (trie_exemple, (['d'; 's'; 'i'; 't'; 'e'; '$'], false));
  (trie_exemple, (['d'; 'o'; 'd'; 'o'; '$'], false));
]

let insere_tests = [
  (trie_exemple, (['d'; 'e'; 'p'; 'u'; 'i'; 's'; '$'], [['d'; 'o'; '$']; ['d'; 'o'; 'l'; 'e'; '$']; ['d'; 'o'; 'd'; 'u'; '$'];['d'; 'o'; 'd'; 'o'; 's'; '$']; ['d'; 'e'; '$']; ['s'; 'i'; '$'];['s'; 'i'; 't'; 'e'; '$']; ['s'; 'i'; 'r'; 'e'; '$']; ['s'; 'a'; 'c'; '$'];['s'; 'k'; 'i'; '$'];['d'; 'e'; 'p'; 'u'; 'i'; 's'; '$']]))
]

let trie_of_list_tests = [
  (exemple, trie_exemple);
  (exemple2, trie_exemple2);
  (["a"; "ac"; "b"], N('a', N('$', V, N('c', N('$', V, V), V)), N('b', N('$', V, V), V)))
]

let longueur_max_tests = [
  (trie_exemple, 5);
  (N('a', N('$', V, N('c', N('$', V, V), V)), N('b', N('$', V, V), V)), 2);
  (trie_exemple2, 8);
]

let compte_mots_longs_tests = [
  (trie_exemple, (4, 5));
  (trie_exemple2, (6, 12));
]

let tableau_occurences_tests = [
  ("bananes", [|2; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 2; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0|]);
  ("azerty", [|1; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 0; 0; 1; 1|]);
  ("qwerty", [|0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 1; 0; 1; 0|]);
  ("abcdefghijklmnopqrstuvwxyz", [|1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1|]);
]

(* Tests *)
let from_bool (value: bool) = if value then "true" else "false"

(* Support pour Ansi (Stack Overflow) *)
module Ansi = struct
  let esc = "\x1b["

  let reset     = esc ^ "0m"
  let bold      = esc ^ "1m"
  let underline = esc ^ "4m"

  let black     = esc ^ "30m"
  let red       = esc ^ "31m"
  let green     = esc ^ "32m"
  let yellow    = esc ^ "33m"
  let blue      = esc ^ "34m"
  let magenta   = esc ^ "35m"
  let cyan      = esc ^ "36m"
  let white     = esc ^ "37m"
end

(*
Test la fonction `f` dans un unit test de nom `name`.
Compare la sortie de `f fst(test_list)` et `snd(test_list)` avec `eq`.

Pour des raisons de variadicité (variabilité de l'arité), eq prend deux entrées
de types différents (car 'b peut être le type d'une fonction).
*)
let test_f (f: 'a -> 'b) (name: string) (test_list: ('a * 'c) list) (eq: 'b -> 'c -> bool) = 
  let rec inner_test (ts: ('a * 'c) list) (counter: int) (succ: int) = 
    match ts with
    | [] -> true, counter, succ
    | (arg, expectation)::q -> 
      let value = f arg in (* TODO: print v on failure *)
      if not (eq (value) (expectation)) then
      begin
        Printf.printf "    %sFAIL: %s on test n°%d\n" (Ansi.bold ^ Ansi.red) name counter;
        let _, c, s = inner_test q (counter + 1) succ in
        false, c, s
      end
      else
        inner_test q (counter + 1) (succ + 1)
  in
  printf "%sTesting `%s`:\n" (Ansi.bold ^ Ansi.cyan) name;
  let r, count, succeeded = inner_test test_list 0 0 in
  printf "%sends with %d/%d%s.\n\n" 
    (Ansi.bold ^ if succeeded = count then Ansi.yellow else Ansi.magenta) 
    succeeded 
    count 
    (Ansi.reset)
  ; 
  r

(*
-------------------------------------------------------------------------------
EXERCICE 1
-------------------------------------------------------------------------------
*)

(* QU 1:
est_bien_forme : trie -> bool *)

let rec est_bien_forme (a: trie) =
  match a with
  | V -> true
  | N('$', V, d) -> est_bien_forme d
  | N('$', _, _) -> false
  | N(_, V, _) -> false
  | N(_, g, d) -> est_bien_forme g && est_bien_forme d

(* QU 2:
mot_of_string : string -> mot *)

let mot_of_string (s: string) =
  let res = ref ['$'] in
  for i = String.length s - 1 downto 0 do
    res := s.[i]::!res
  done;
  !res

(* QU 3:
afficher_mot : mot -> unit *)

let rec afficher_mot (m: mot) = 
  match m with
  | [] -> ()
  | '$'::q -> print_char '\n'
  | t::q -> print_char t; afficher_mot q

let _ = afficher_mot ['B'; 'o'; 'n'; 's'; 'o'; 'i'; 'r'; ' '; '!'; '$']
let _ = print_newline ()

(* QU 4:
mots_of_trie : trie -> mot list *)

let rec mots_of_trie (t: trie) = 
  match t with
  | V -> []
  | N('$', V, d) -> ['$']::(mots_of_trie d)
  | N(c, g, d) -> (concat_on_all c (mots_of_trie g)) @ mots_of_trie d
and concat_on_all (c: char) (l: mot list) = 
  match l with
  | [] -> []
  | t::q -> (c::t)::concat_on_all c q

(*
-------------------------------------------------------------------------------
EXERCICE 2
-------------------------------------------------------------------------------
*)

(* QU 1:
cardinal : trie -> int *)

let rec cardinal (t: trie) = 
  match t with
  | V -> 0
  | N('$', V, d) -> 1 + cardinal d
  | N(_, g, d) -> cardinal g + cardinal d

(* QU2:
recherche : trie -> mot -> bool *)

let rec recherche (t: trie) (m: mot) =
  match m with
  | [] -> true
  | hd::q ->
    match t with
    | V -> false
    | N('$', V, d) -> (hd = '$') || recherche d m
    | N(c, g, d) -> ((c = hd) && recherche g q) || (recherche d m)

(* QU 3: Justifier grâce à la Remarque 3 que cette complexité est en fait en 𝒪(𝑛) avec 𝑛 la
    taille du mot recherché. Quelle est la complexité dans le meilleur cas ?

Un variant pour recherche est la taille du trie que l'on explore. Ainsi, naïvement,
on majore la complexité de recherche avec cette dernière ce qui nous fait dire que 
recherche est en O(h) avec h la hauteur du trie. 
Cependant, dès que le mot est trouvé, ou que le trie vide, on peut conclure.
Si le trie est vide avant que toutes les lettres du mot n'est été trouvées, on peut affirmer, avec l'hypothèse 
de la Remarque 3, que le mot n'est pas représenté par le trie.

A chaque appel de recherche, on compare la lettre du mot à laquelle on est rendu avec
l'étiquette du nœud. 
Si les deux correspondent, on cherche les lettres suivantes dans le sous-trie gauche,
sinon, dans le sous-trie droit. 
En effet, l'hypothèse de la Remarque 3, on affirme que procéder ainsi nous assure 
que si le début du mot est représenté dans le sous-arbre gauche, ce même début de mot ne peut 
être représenter dans le sous-trie droit.

Conclusion: la complexité dépend de la taille du mot, donc recherche est en O(n).

La complexité dans le meilleur cas est en O(n), car que le mot soit représenté ou non par le trie,
on devra égrainé le trie pour s'en assurer.
*)

(* QU 4:
insere : trie -> mot -> trie *)

let rec insere (t: trie) (m: mot) = 
  match m with
  | [] -> t
  | hd::q ->
    match t with
    | V -> N(hd, insere V q, V)
    | N('$', V, d) when hd = '$'-> t
    | N('$', V, d) -> N('$', V, insere d m)
    | N(c, g, d) when c = hd -> N(c, insere g q, d)
    | N(c, g, d) -> N(c, g, insere d m)

(* QU 5:
trie_of_list : string list -> trie *)

let trie_of_list (liste: string list) =
  let rec aux (l: string list) (t: trie) = 
    match l with
    | [] -> t
    | mot::q ->
      match t with
      | V -> aux q (insere V (mot_of_string mot))
      | _ -> aux q (insere t (mot_of_string mot))
  in
  aux liste V
  
(* QU 6:
longueur_max : trie -> int *)

let rec longueur_max (t: trie) = 
  match t with
  | V -> -1
  | N('$', V, V) -> 0
  | N('$', V, d) -> longueur_max d
  | N(_, g, d) -> max (1+ longueur_max g) (longueur_max d)

(* QU 7:
compte_mots_longs : trie -> int -> int *)

let get_letter (t: trie) = 
  match t with
  | V -> "\b\b"
  | N(c, _, _) -> sprintf "%c" c

let rec compte_mots_longs (t: trie) (n: int) =
  if n > 1 then
    match t with
    | V -> 0
    | N('$', V, d) -> compte_mots_longs d n
    | N(_, g, d) -> compte_mots_longs g (n - 1) + compte_mots_longs d n
  else 
    if (get_letter t) = "$" && n = 1 then
      0
    else
      match t with
      | V -> 0
      | N('$', V, d) -> 1 + compte_mots_longs d (n-1)
      | N(_, g, d) -> compte_mots_longs g (n-1) + compte_mots_longs d (n-1)

(* QU 8:
iter_trie : (mot -> unit) -> trie -> unit *)

let iter_trie (f: mot -> unit) (t: trie) = 
  let l = mots_of_trie t in
  let rec aux (liste: mot list) = 
    match liste with
    | [] -> ()
    | t::q -> f t; aux q
  in aux l

let _ = iter_trie (afficher_mot) trie_exemple2; print_newline ()

(* QU 9:
affiche_mots : trie -> unit
list_of_trie : trie -> mot list *)

let affiche_mots (t: trie) = 
  iter_trie afficher_mot t

let list_of_trie (t: trie) =
  let l = ref [] in
  (iter_trie (fun m -> (l := m::!l)) t);
  !l

let _ = affiche_mots trie_exemple2
let _ = print_newline ()
let _ = List.map afficher_mot (list_of_trie trie_exemple2)
let _ = print_newline ()

(* QU 10:
tableau_occurences : string -> int array *)

let rec tableau_occurences (s: string) = 
  let occ = Array.init 26 (fun _ -> 0) in
  for i = 0 to String.length s - 1 do
    let indice = int_of_char s.[i] - 97 in
    occ.(indice) <- occ.(indice) + 1
  done;
  occ

(*
-------------------------------------------------------------------------------
EXERCICE 3
-------------------------------------------------------------------------------
*)

(* QU 1:
cat_first_line : string -> unit *)

let cat_first_line (filename: string) = 
  try
    let file = open_in filename in 
    try
      let content = input_line file in
      printf "%s\n" content;
      close_in file
    with End_of_file ->
      close_in file
  with Sys_error message -> printf "%s" message

let _ = cat_first_line "cinq_cent_mots.txt"
let _ = print_newline ()

(* QU 2:
cat_first_100_lines : string -> unit *)

let cat_first_100_lines (filename: string) =
  let rec cat_n_lines (file: in_channel) (n: int) =
    if n = 0 then
      ()
    else
      try
        let content = input_line file in
        printf "%s\n" content; 
        cat_n_lines file (n-1)
      with End_of_file ->
        printf "Fichier terminé !"
  in 
    try
      let file = open_in filename in 
        cat_n_lines file 10;
        close_in file
    with Sys_error (message: string) -> 
      printf "%s" message

let _ = cat_first_100_lines "cinq_cent_mots.txt"
let _ = print_newline ()

(* QU 3:
cat : string -> unit *)

let cat (filename: string) = 
  let rec cat_until_eof (file: in_channel) =
    try
      let content = input_line file in
      printf "%s\n" content;
      cat_until_eof file
    with End_of_file ->
      close_in file
  in
    try
      let file = open_in filename in 
        cat_until_eof file
    with Sys_error (message: string) -> 
      printf "%s" message

let _ = cat "cinq_cent_mots.txt"
let _ = print_newline ()

(* QU 4:
trie_of_file : string -> trie *)

let rec trie_of_file (filename: string) = 
  let rec aux (file: in_channel) (t: trie) =
    try
      let content = input_line file in
      aux file (insere t (mot_of_string content))
    with End_of_file ->
      close_in file;
      t
  in
    try
      let file = open_in filename in 
        aux file V
    with Sys_error (message: string) -> 
      printf "%s" message; 
      V

let cinq_cents_mots = trie_of_file "cinq_cent_mots.txt"
let ods6_lowercase = trie_of_file "ods6_lowercase.txt"

(*
-------------------------------------------------------------------------------
EXERCICE 4
-------------------------------------------------------------------------------
*)

(* QU 1:
sous_mots : trie -> string -> unit *)

(* QU 2:
afficher_anagrammes : trie -> string -> unit *)

(* QU 3:
filtrer_sous_mots : trie -> string -> trie
filtrer_anagrammes : trie -> string -> trie *)

(* QU 4:
filtrer_sur_mots : trie -> string -> trie *)

(* QU 5:
affiche_decomposition : trie -> string -> unit *)

(* QU 6:
filtrer_decompositions : trie -> string -> trie *)

(* QU 7:
filtrer_decompositions_uniques : trie -> string -> trie *)

(*
-------------------------------------------------------------------------------
EXERCICE 5
-------------------------------------------------------------------------------
*)

(* ... *)

(*
-------------------------------------------------------------------------------
TESTS
-------------------------------------------------------------------------------
*)

(* 1.1 *)
let x = test_f est_bien_forme "est_bien_forme" est_bien_forme_tests (=)
let _ = if enabled then assert x

(* 1.2 *)
let x = test_f mot_of_string "mot_of_string" mot_of_string_tests eq_mot
let _ = if enabled then assert x

(* 1.3 *)

(* 1.4 *)
let x = test_f mots_of_trie "mots_of_trie" mots_of_trie_tests eq_mot_list
let _ = if enabled then assert x

(* 2.1 *)
let x = test_f cardinal "cardinal" cardinal_tests (=)
let _ = if enabled then assert x

(* 2.2 *)
let x = test_f recherche "recherche" recherche_tests (comp_partial (=))
let _ = if enabled then assert x

(* 2.4 *)
let x = test_f insere "insere" insere_tests (comp_partial (fun t l -> eq_mot_list (mots_of_trie t) l))
let _ = if enabled then assert x

(* 2.5 *)
let x = test_f trie_of_list "trie_of_list" trie_of_list_tests (fun t1 t2 -> eq_mot_list (mots_of_trie t1) (mots_of_trie t2))
let _ = if enabled then assert x

(* 2.6 *)
let x = test_f longueur_max "longueur_max" longueur_max_tests (=)
let _ = if enabled then assert x

(* 2.7 *)
let x = test_f compte_mots_longs "compte_mots_longs" compte_mots_longs_tests (comp_partial (=))
let _ = if enabled then assert x

(* 2.8 *)

(* 2.9 *)

(* 2.10 *)
let x = test_f tableau_occurences "tableau_occurences" tableau_occurences_tests (=)
let _ = if enabled then assert x

(* 3.1 *)

(* 3.2 *)

(* 3.3 *)

(* 3.4 *)
let x = (cardinal cinq_cents_mots) == 471
let _ = if enabled then assert x

let x = (cardinal ods6_lowercase) == 386264
let _ = if enabled then assert x
