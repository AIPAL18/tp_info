open Elie_ruggiero
open Printf

(* ----------------------------------------------------------------------------
Préambule
---------------------------------------------------------------------------- *)

let rec arbre_braun (size: int): int braun = 
  if size = 0 then
    V
  else
    N(
      size, 
      arbre_braun (size / 2), 
      arbre_braun ((size - 1) / 2)
    )

let get_n_spaces (n: int): string = 
  let s = ref "" in
  for i = 0 to n - 1 do
    s := !s ^ " "
  done;
  !s

let m_log2 x =
  let rec aux n acc =
    if n <= 1 then acc else aux (n lsr 1) (acc + 1)
  in aux x 0

let coucou (a: 'a braun) =
  let q = Queue.create () in 
  Queue.add a q;
  let values = Queue.create () in
  let rec braun_to_list () =
    if not (Queue.is_empty q) then(
      match Queue.pop q with
      | V -> Queue.push None values
      | N(e, g, d) ->
        Queue.push (Some e) values;
        Queue.push g q;
        Queue.push d q;
        braun_to_list ()
    )
  in
  braun_to_list ();
  Array.of_seq (Queue.to_seq values)
  
let display (a: 'a braun) (display_label: 'a option -> unit): unit =
  if is_empty a then
    printf "V\n"
  else 
  let t = coucou a in
  let level = ref (-1) in
  let height = m_log2 (Array.length t) in
  printf "taille: %d\n\n" (Array.length t);
  let size_of_element = 3 in

  for i = 0 to (Array.length t) - 1 do
    if i = 0 || not ((i + 1 >= (1 lsl !level)) && (i + 1 <= ((1 lsl (!level + 1)) - 1))) then begin
      incr level;
      print_newline ();
      for _ = 1 to size_of_element * ((1 lsl (height - !level)) - 1) do
        print_char ' '
      done
    end;
    display_label (t.(i));
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

let affiche (x: int option): unit = 
  match x with
  | None -> printf " V "
  | Some(e) -> printf "%03d" e
let _ = 
  for i = 0 to 20 do
    let a = arbre_braun i in
    display a affiche
  done

