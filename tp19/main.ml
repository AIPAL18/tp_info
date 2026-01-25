(* ----------------------------------------------------------------------------
Imports
---------------------------------------------------------------------------- *)
open Printf

(* ----------------------------------------------------------------------------
Types
---------------------------------------------------------------------------- *)
type bit = Z | U
type nombre = bit list
type 'a arbre = F of 'a | N of int * 'a arbre * 'a arbre
type 'a liste_binaire = 'a arbre option list

(* ----------------------------------------------------------------------------
Exemples
---------------------------------------------------------------------------- *)
let zero = []
let six = [Z; U; U] (* bit de poids faible en tête *)

let ex = N (4, N(2, F 0.5, F (-1.2)), N (2, F 5.2, F 3.6)) (* : float arbre *)

let ex2 = [
  Some(F(0.5));
  Some(N(2, F(4.2), F(5.1)));
  None;
  Some(N(8, N(4, N(2, F(-1.2), F(3.6)), N(2, F(0.5), F(-1.6))), N(4, N(2, F(-10.5), F(0.5)), N(2, F(10.0), F(0.0)))));
]


(* ----------------------------------------------------------------------------
Tests
---------------------------------------------------------------------------- *)
let display_num (l: bit list) =
  List.iter (fun x -> match x with | U -> print_string "1" | Z -> print_string "0") (List.rev l);
  print_string "\n"

let rec from10 (n: int) =
  if n < 0 then failwith "Negative number cannot be represented"
  else if n = 0 then
    [Z] 
  else if n = 1 then
    [U]
  else if n mod 2 = 0 then
    Z::from10(n/2)
  else
    U::from10(n/2)

let pow2 n =
  if n = 0 then 1
  else if n > 0 then 2 lsl (n-1)
  else failwith "Negative power !"

let from2 (n: nombre) =
  let rec aux (nb: nombre) (pos: int) =
    match nb with
    | [] -> 0
    | Z::q -> aux q (pos+1)
    | U::q -> pow2 pos + aux q (pos+1)
  in aux n 0

let rec get_arbre (i: int) (a: 'a arbre) = 
  match a with
  | F(e) -> if i = 0 then e else failwith "Indice overflow"
  | N(n, g, d) -> if i < (n/2) then get_arbre (i) g else get_arbre (i-(n/2)) d
let rec get (u: 'a liste_binaire) (i: int) = 
  match u with
  | [] -> failwith "Index overflow"
  | None::q -> get q i
  | Some(F(e))::q -> if i = 0 then e else get q (i-1)
  | Some(N(n, g, d))::q ->
    if i >= n then
      get q (i-n)
    else
      get_arbre i (N(n, g, d))

let list_from_arbre ?(ml: 'a list = []) (a: 'a arbre) = 
  let rec aux (u: 'a arbre) (l: 'a list) = 
    match u with
    | F(e) -> e::l
    | N(_, g, d) -> aux g (aux d l)
  in aux a ml

let list_from_liste_binaire ?(ml: 'a list = []) (a: 'a liste_binaire) = 
  let rec aux (u: 'a liste_binaire) (l: 'a list) = 
    match u with
    | [] -> l
    | None::q -> aux q l
    | Some(F(e))::q -> e::aux q l
    | Some(N(_, g, d))::q -> list_from_arbre g ~ml:(list_from_arbre d ~ml:(aux q l))
  in aux a ml
      
let rec display_arbre (dis: 'a -> unit) (a: 'a arbre) =
  match a with
  | F(e) -> dis e
  | N(_, g, d) -> display_arbre dis g; display_arbre dis d
      
let display (dis: 'a -> unit) (a: 'a liste_binaire) =
  let print (e: 'a) = dis e; print_string ", "
  in
  let rec aux (u: 'a liste_binaire) =   
    match u with
    | [] -> ()
    | None::q -> aux q
    | Some(F(e))::q -> print e; aux q
    | Some(N(_, g, d))::q -> display_arbre print g ; display_arbre print d; aux q
  in
  printf "[";
  aux a;
  printf "]\n"

(* ----------------------------------------------------------------------------
EXERCICE 1
---------------------------------------------------------------------------- *)

(* QU 1:
succ : nombre -> nombre *)
let rec succ (n: nombre) = 
  match n with
  | [] -> [U]
  | Z::q -> U::q
  | U::q -> Z::succ q

let () = Tests.pa __POS__ ((succ six) = [U; U; U])
let () = Tests.pa __POS__ ((succ six) = [U; U; U])
let () = Tests.pa __POS__ ((from10 6) = six)
let () = Tests.pa __POS__ (from2 six = 6)

let () = Tests.pa __POS__  (from2 (succ (from10 10)) = 11)
  

(* QU 1:
pred : nombre -> nombre *)

let rec pred (n: nombre) = 
  match n with
  | [] | Z::[] -> failwith "Negative number cannot be represented"
  | Z::(U::q) -> U::Z::q
  | Z::q -> U::pred q
  | U::q -> Z::q

let to_true e = ignore e; true
let () = 
  for i = 0 to 40 do
    Tests.pa __POS__  (from2 (succ (from10 i)) = (i+1) && to_true i)
  done

(* ----------------------------------------------------------------------------
EXERCICE 2
---------------------------------------------------------------------------- *)
  
(* QU 1:
get_arbre : int -> 'a arbre -> 'a *)
let rec get_arbre (i: int) (a: 'a arbre) = 
  match a with
  | F(e) -> if i = 0 then e else failwith "Indice overflow"
  | N(n, g, d) -> if i < (n/2) then get_arbre (i) g else get_arbre (i-(n/2)) d

let () = Tests.pa __POS__ (get_arbre 0 ex = 0.5)
let () = Tests.pa __POS__ (get_arbre 1 ex = (-1.2))
let () = Tests.pa __POS__ (get_arbre 2 ex = 5.2)
let () = Tests.pa __POS__ (get_arbre 3 ex = 3.6)

(* set_arbre : int -> 'a -> 'a arbre -> 'a arbre *)
let rec set_arbre (i: int) (x: 'a) (a: 'a arbre) =
  match a with
  | F(e) -> if i = 0 then F(x) else failwith "Index overflow"
  | N(n, g, d) -> 
    if i < (n/2) then 
      N(n, set_arbre (i) x g, d) 
    else 
      N(n, g, set_arbre (i-(n/2)) x d)

let () = Tests.pa __POS__ (get_arbre 3 (set_arbre 3 10.0 ex) = 10.0)

(* QU 3:
Soit 𝑢 une liste binaire non vide, 𝑛 > 0 sa taille et 𝑘 sa longueur. Exprimer 𝑘 en
fonction de 𝑛. (Longueur: k=4, Taille: n=|u|=11)

-> k = ceil(log(n+1, 2))
*)

(* QU 4:
get : 'a liste_binaire -> int -> 'a *)
let rec get (u: 'a liste_binaire) (i: int) = 
  match u with
  | [] -> failwith "Index overflow"
  | None::q -> get q i
  | Some(F(e))::q -> if i = 0 then e else get q (i-1)
  | Some(N(n, g, d) as a)::q ->
    if i >= n then
      get q (i-n)
    else
      get_arbre i a

let () = Tests.pa __POS__  ((get ex2 10) = 0.0)

(* QU 5:
Exprimer la complexité dans le pire cas de get u k en fonction de |𝑢|. Est-ce que
l’on a une complexité en 𝒪(1) en fixant 𝑘 = 0 ?

Les appels récursifs de get est en $\cal O(\lceil\log_2(|u|+1)\rceil)$ puis get_arbre est en $\cal O(|u|)$.
Or l'appel à get_arbre n'est effectué qu'à la fin.

D'où get en $\cal O(|u| + \log_2(|u|))$ i.e. $\cal O(\log(n))$

Pour k = 1:
Le pire cas: |u| est une puissance de 2. Dans ce cas, la liste ne représente 
qu'un arbre, le dernier. 
D'où $\cal O(\log(n))$
*)

(* QU 6:
set : 'a liste_binaire -> int -> 'a -> 'a liste_binaire *)
let rec set (u: 'a liste_binaire) (i: int) (x: 'a) = 
  match u with
  | [] -> failwith "Index overflow"
  | None::q -> None::set q i x
  | Some(F(e))::q -> if i = 0 then Some(F(x))::q else Some(F(e))::set q (i-1) x
  | Some(N(n, g, d))::q ->
    if i >= n then
      Some(N(n, g, d))::set q (i-n) x
    else
      Some((set_arbre i x (N(n, g, d))))::q


let () = Tests.pa __POS__  (get (set ex2 10 10.0) 10 = 10.0)

(* QU 7:
cons : 'a -> 'a liste_binaire -> 'a liste_binaire *)

let merge_arbre (a1: 'a arbre) (a2: 'a arbre) = 
  match a1, a2 with
  | F(_), F(_) -> N(2, a1, a2)
  | N(n1, _, _), N(n2, _, _) when n1 = n2 -> N(n1+n2, a1, a2)
  | N(_, _, _), N(_, _, _)
  | F(_), N(_, _, _)
  | N(_, _, _), F(_) -> failwith "Cannot merge trees of different formats."

let rec cons_arbre (e: 'a) (l: 'a liste_binaire) (ar: 'a arbre) = 
  match l with
  | [] -> [Some(F(e))]
  | None::q -> Some(ar)::q
  | Some(a)::q -> None::cons_arbre e q (merge_arbre ar a)

let cons (e: 'a) (l: 'a liste_binaire) =
  cons_arbre e l (F(e))

let () = display print_float ex2
let () = display print_float (cons 0.1 ex2)
let () = display print_float (cons 10. (cons 0.1 ex2))

(* QU 8:
Déterminer la complexité dans le pire et le meilleur cas de cons. *)

(* QU 9:
uncons : 'a liste_binaire -> 'a * 'a liste_binaire
head : 'a liste_binaire -> 'a
tail : 'a liste_binaire -> 'a liste_binaire *)

(* let rec pred (n: nombre) = 
  match n with
  | [] | Z::[] -> failwith "Negative number cannot be represented"
  | Z::(U::q) -> U::Z::q
  | Z::q -> U::pred q
  | U::q -> Z::q *)

let () = List.iter (printf "%.1f, ") (list_from_arbre ex)
let () = print_newline ()

let () = List.iter (printf "%.1f, ") (list_from_liste_binaire ex2)
let () = print_newline ()

