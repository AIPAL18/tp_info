open Printf

type candidat = {
  mutable k : int;
  placement : int array
}

let a_rejeter (c: candidat): bool = 
  let i = ref 0 and b = ref true in
  while !i < c.k - 1 && !b do
    b := c.placement.(!i) <> c.placement.(c.k - 1) && abs (c.placement.(!i) - c.placement.(c.k - 1)) <> c.k - 1 - !i;
    incr i
  done;
  not !b

(* fixer_nouvelle_reine : candidat -> int -> unit *)
let fixer_nouvelle_reine (c: candidat) (j: int): unit =
  c.placement.(c.k) <- j;
  c.k <- c.k + 1

(* abandonne_derniere_reine : candidat -> unit *)
let abandonne_derniere_reine (c: candidat): unit =
  c.k <- c.k - 1

let solution_n_reines_opt (n: int): candidat option =
  let rec parcours (c: candidat) = 
    if a_rejeter c then None
    else if c.k = n then Some c
    else
      let rec loop (i: int) = 
        if i >= n then None
        else (
          fixer_nouvelle_reine c i;
          match parcours c with
          | None -> abandonne_derniere_reine c; loop (i+1)
          | Some c -> Some c
        )
      in loop 0
  in parcours {k = 0; placement = Array.make n 0}

let _ = 
  let s = solution_n_reines_opt 8 in
  printf "[|";
  if Option.is_some s then
    Array.iter (fun x -> printf "%d, " x) (Option.get s).placement;
  printf "|]\n";

  