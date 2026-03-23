open Printf
open Element

type couleur = Rouge | Noir
type 'a arn = V | N of couleur * 'a arn * 'a * 'a arn

let rec min_arn (a: 'a arn): 'a = 
  match a with
  | V -> failwith "empty tree"
  | N(_, V, e, _) -> e
  | N(_, g, _, _) -> min_arn g 

let rec max_arn (a: 'a arn): 'a = 
  match a with
  | V -> failwith "empty tree"
  | N(_, _, e, V) -> e
  | N(_, _, _, d) -> max_arn d 

let rec recherche (a: 'a arn) (x: 'a): bool = 
  match a with
  | V -> false
  | N(_, g, e, d) ->
    if x = e then
      true
    else if x < e then
      recherche g x
    else
      recherche d x

(* ----------------------------------------------------------------------------
Exercice 2
---------------------------------------------------------------------------- *)

(* remonte_rouge : 'a arn -> 'a arn *)
let remonte_rouge (a: 'a arn): 'a arn = 
  match a with
  | N(Noir, N(Rouge, N(Rouge, a1, x, a2), y, a3), z , a4)
  | N(Noir, N(Rouge, a1, x, N(Rouge, a2, y, a3)), z , a4)
  | N(Noir, a1, x, N(Rouge, N(Rouge, a2, y, a3), z, a4))
  | N(Noir, a1, x, N(Rouge, a2, y, N(Rouge, a3, z, a4)))
    -> N(Rouge, N(Noir, a1, x, a2), y, N(Noir, a3, z, a4))
  | V | N(_, _, _, _) -> a

(* insere_aux : 'a arn -> 'a -> 'a arn *)
let rec insere_aux (a: 'a arn) (x: 'a): 'a arn = 
  match a with
  | V -> N(Noir, V, x, V)
  | N(c, g, e, d) ->
    if x = e then
      a
    else if x < e then
      remonte_rouge (N(c, insere_aux g x, e, d))
    else
      remonte_rouge (N(c, g, e, insere_aux d x))

let is_empty (a: 'a arn): bool = 
  match a with
  | V -> true
  | _ -> false
      
let rec hauteur (a: 'a arn): int = 
  match a with
  | V -> -1
  | N(_, g, _,  d) -> 1 + max (hauteur g) (hauteur d)

type 'a couple = C of couleur * 'a
  
let display (a: 'a arn) (display_label: 'a couple -> unit): unit =
  if is_empty a then
    printf "V\n"
  else 
  let m_log2 x =
    let rec aux n acc =
      if n <= 1 then acc else aux (n lsr 1) (acc + 1)
    in aux x 0
  in
  let get_root (arb: 'a arn): 'a couple element = 
    match a with
    | V -> Empty
    | N(c, _, e, _) -> Element(C(c, e)) in
  let t = Array.make ((1 lsl (hauteur a + 1)) - 1) (Element.Empty) in
  t.(0) <- get_root a;
  let rec abr_to_list (abr: 'a arn) (acc: int): unit =
    match abr with
    | V -> ()
    | N(c, g, e, d) -> 
      if 2*acc + 1 < Array.length t then (
        t.(2*acc + 1) <- get_root g;
        abr_to_list g (acc + 1)
      )
      else if 2*acc + 2 < Array.length t then (
        t.(2*acc + 2) <- get_root d;
        abr_to_list d (acc + 2)
      )
    in
  abr_to_list a 0;
  let level = ref (-1) in
  let height = m_log2 (Array.length t) in
  Printf.printf "taille: %d\n\n" (Array.length t);
  let size_of_element = 2 in

  for i = 0 to (Array.length t) - 1 do
    if i = 0 || not ((i + 1 >= (1 lsl !level)) && (i + 1 <= ((1 lsl (!level + 1)) - 1))) then begin
      incr level;
      print_newline ();
      for _ = 1 to size_of_element * ((1 lsl (height - !level)) - 1) do
        print_char ' '
      done
    end;
    if (Element.is_element t.(i)) then
      display_label  ((Element.get_element t.(i)))
    else
      Printf.printf " V ";
    for _ = 1 to size_of_element * ((1 lsl (height - (!level - 1))) - 1) do
      print_char ' '
    done
  done;
  print_newline ()

let _ = display 
