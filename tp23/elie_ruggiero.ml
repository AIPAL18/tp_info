type 'a braun = V | N of 'a * 'a braun * 'a braun

let rec diff (a : 'a braun) (n : int) : int =
  (* 
  On a
    #   2m   <= |A| <= 2m + 1 donc |A| = 2m + 1 <-> |g| = m   et |d| = m
                                   |A| = 2m     <-> |g| = m   et |d| = m-1
    # 2m + 1 <= |A| <= 2m + 2 donc |A| = 2m + 1 <-> |g| = m   et |d| = m
                                   |A| = 2m     <-> |g| = m+1 et |d| = m
  D'où
  *)
  match a with
  | V  when n = 0 -> 0
  | N(_, V, V)  when n = 0 -> 1
  | N(_, g, d) -> 
    if n mod 2 = 1 then
      diff g ((n - 1)/2)
    else
      diff d ((n - 2)/2)
  | V -> assert false
      
let rec taille (a : 'a braun) : int = 
  match a with
  | V -> 0
  | N(_, g, d) -> 
    let t = taille d in 
    1 + 2 * t + diff g t

let rec hauteur (a : 'a braun) : int =
  match a with
  | V -> -1
  | N(_, g, _) -> 1 + hauteur g

let create : int braun = V

let is_empty (a : 'a braun) : bool = 
  match a with
  | V -> true
  | N(_, _, _) -> false

let get_min (a : int braun) : int = 
  match a with
  | V -> max_int
  | N(e, _, _) -> e

let rec insert (a : int braun) (x : int) : int braun = 
  match a with
  | V -> N(x, V, V)
  | N(e, g, d) -> 
    if (taille g) = (taille d) then
      N(e, insert g x, d)
    else
      N(e, g, insert d x)

let rec extract_element (a : int braun) : int * int braun =
  failwith "TODO: extract_element"

let rec replace_min (a : int braun) (x : int) : int braun =
  failwith "TODO: replace_min"

let rec merge (g : int braun) (d : int braun) : int braun =
  failwith "TODO: merge"

and extract_min (a : int braun) : int * int braun = failwith "TODO: extract_min"

(* ----------------------------------------------------------------------------
---------------------------------------------------------------------------- *)
open Printf

let rec arbre_braun (size: int): int braun = 
  if size = 0 then
    V
  else
    N(
      size, 
      arbre_braun (size / 2), 
      arbre_braun ((size - 1) / 2)
    )


let _ = 
  for i = 0 to 20 do
    printf "Braun de taille: %d\n" i;
    flush stdout;
    let a = arbre_braun i in

    printf "\t1, ";
    printf "diff: %d = 0" (diff a i);
    assert(diff a i = 0);
    if i > 0 then (
      printf "\n\t   diff: %d = 1" (diff a (i - 1));
      assert(diff a (i - 1) = 1);
    );

    printf "\n\t2, ";
    printf "taille: %d = %d" (taille a) i;
    assert(taille a = i);

    printf "\n\t3, ";
    if i > 0 then (
      printf "hauteur: %d = %d" (hauteur a) (int_of_float (Float.log2 (float_of_int i)));
      assert(hauteur a = int_of_float (Float.log2 (float_of_int i)))
    );
    printf "\n\n"
  done
    