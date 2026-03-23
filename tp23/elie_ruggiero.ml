(* ############################################################################
Modules / Types
############################################################################ *)
open Printf
type 'a braun = V | N of 'a * 'a braun * 'a braun

(* ----------------------------------------------------------------------------
Exercice 5
---------------------------------------------------------------------------- *)

let rec diff (a : 'a braun) (n : int) : int =
  (* 
  On a, par la question 1 et 2:
    #   2m   <= |A| <= 2m + 1 donc |A| = 2m + 1 <-> |g| = m   et |d| = m
                                   |A| = 2m     <-> |g| = m   et |d| = m-1
    # 2m + 1 <= |A| <= 2m + 2 donc |A| = 2m + 1 <-> |g| = m   et |d| = m
                                   |A| = 2m     <-> |g| = m+1 et |d| = m
  D'où A=N(g,d) de taille n
    |g| = ⌈(n-1)/2⌉ = ceil((n-1)/2)
    |d| = ⌊(n-1)/2⌋ = floor((n-1)/2)
  Donc si n est impair
    |d| = (n-1)/2
    et |g| = (n-1)/2 ou |g| = (n+1)/2
    donc |g| = |d| + diff |g| (n-1)/2
  De même pour n pair
    |g| = (n-1)/2
    |d| = (n-2)/2 ou |d| = n/2
    |d| = |g| + diff |g| (n-2)/2
  
  Ainsi
  diff N(g, d) (2k + 1) -> diff g (n-1)/2
  diff N(g, d) (2k + 2) -> diff d (n-2)/2
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
  (*
  Cas de base: 
    a = V -> |a| = 0
  Cas général: 
    a = N(g, d)
    |a| = |g| + |d|
        = 2*|g|     si |g| = |d|
        = 2*z
  *)
  match a with
  | V -> 0
  | N(_, g, d) -> 
    let t = taille d in 
    1 + 2 * t + diff g t

(* 
diff est en O(log n)
On fait h(A) appelle à taille Or h(A) = ⌊log |A|⌋ = O(log n)
La complexité de taille est en O(log²n)
*)
    
let rec hauteur (a : 'a braun) : int =
  match a with
  | V -> -1
  | N(_, g, _) -> 1 + hauteur g

let create : int braun = V

(* ----------------------------------------------------------------------------
Exercice 6
---------------------------------------------------------------------------- *)

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
    if (taille g) = (taille d) then (
      let vf, vc, g', d' = match insert g x with
              | V -> assert false
              | N(e', g', d') -> 
                max e e', min e e', g', d'
      in
      N(vf, N(vc, g', d'), d);
    )
    else (
      let vf, vc, g', d' = match insert d x with
              | V -> assert false
              | N(e', g', d') -> 
                max e e', min e e', g', d'
      in
      N(vf, g, N(vc, g', d'));
    )

let rec extract_element (a : int braun) : int * int braun =
  failwith "TODO: extract_element"

let rec replace_min (a : int braun) (x : int) : int braun =
  failwith "TODO: replace_min"

let rec merge (g : int braun) (d : int braun) : int braun =
  failwith "TODO: merge"

and extract_min (a : int braun) : int * int braun = 
  failwith "TODO: extract_min"

(* ----------------------------------------------------------------------------
Tests
---------------------------------------------------------------------------- *)

let rec arbre_braun (size: int): int braun = 
  (* 
  Construit l'arbre de Braun de taille `size` avec la relation de récurrence
  A(0) = V
  A(n) = N(A(⌊(n)/2⌋), A(⌊n-1/2⌋))
  *)
  if size = 0 then
    V
  else
    N(
      size, 
      arbre_braun (size / 2), 
      arbre_braun ((size - 1) / 2)
    )

let m_log2 x =
  let rec aux n acc =
    if n <= 1 then acc else aux (n lsr 1) (acc + 1)
  in aux x 0

let braun_of_array (a: 'a braun) =
  let q = Queue.create () in 
  Queue.add a q;
  let values = Queue.create () in
  let rec queue_of_braun () =
    if not (Queue.is_empty q) then(
      match Queue.pop q with
      | V -> Queue.push None values
      | N(e, g, d) ->
        Queue.push (Some e) values;
        Queue.push g q;
        Queue.push d q;
        queue_of_braun ()
    )
  in
  queue_of_braun ();
  Array.of_seq (Queue.to_seq values)
  
let display (a: 'a braun) ?(size_of_element=3) (display_label: 'a option -> unit): unit =
  if is_empty a then (
    printf "\ntaille: 0\n";
    printf "V\n"
  ) else 
  let braun_tab = braun_of_array a in
  let level = ref (-1) in
  let height = m_log2 (Array.length braun_tab) in
  printf "\ntaille: %d" (taille a);

  for i = 0 to (Array.length braun_tab) - 1 do
    if i = 0 || not ((i + 1 >= (1 lsl !level)) && (i + 1 <= ((1 lsl (!level + 1)) - 1))) then begin
      incr level;
      print_newline ();
      for _ = 1 to size_of_element * ((1 lsl (height - !level)) - 1) do
        print_char ' '
      done
    end;
    display_label (braun_tab.(i));
    for _ = 1 to size_of_element * ((1 lsl (height - (!level - 1))) - 1) do
      print_char ' '
    done
  done;
  print_newline ()

(* ----------------------------------------------------------------------------
Tests
---------------------------------------------------------------------------- *)
  
let _ = 
  for i = 0 to 20 do
    printf "Braun de taille: %d\n" i;
    flush stdout;
    let a = arbre_braun i in

    printf "diff: %d = 0" (diff a i);
    assert(diff a i = 0);
    if i > 0 then (
      printf "\n\t   diff: %d = 1" (diff a (i - 1));
      assert(diff a (i - 1) = 1);
    );

    printf "taille: %d = %d" (taille a) i;
    assert(taille a = i);

    if i > 0 then (
      printf "hauteur: %d = %d" (hauteur a) (int_of_float (Float.log2 (float_of_int i)));
      assert(hauteur a = int_of_float (Float.log2 (float_of_int i)))
    );
    printf "\n"
  done

let affiche (x: int option): unit = 
  match x with
  | None -> printf "   "
  | Some(e) -> printf "%03d" e
let _ = 
  for i = 0 to 20 do
    let a = arbre_braun i in
    display a affiche
  done
