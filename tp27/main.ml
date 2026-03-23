type 'a reponse =
| Refus
| Partiel of 'a
| Ok of 'a

type 'a probleme = {
  c_initial : 'a;
  enfants : 'a -> 'a list;
  analyse_candidat : 'a -> 'a reponse;
}

let enumere_solution (p : 'a probleme) = 
  let rec backtrack c =
    match p.analyse_candidat c with
    | Refus -> []
    | Partiel c' ->
      List.fold_left (* fold_left f init [b1; ...; bn] is f (... (f (f init b1) b2) ...) bn. *)
        (fun acc enfant_c' -> backtrack enfant_c' @ acc)
        [] (p.enfants c')
    | Ok c' -> [c']
  in 
  backtrack p.c_initial

let une_solution_opt p =
  let rec backtrack c =
    match p.analyse_candidat c with
    | Refus -> None
    | Ok c' -> Some c'
    | Partiel c' -> backtrack_list (p.enfants c')
  and backtrack_list = function
    | [] -> None
    | h :: t -> (
      match backtrack h with
      | Some c'' -> Some c''
      | None -> backtrack_list t )
  in
  backtrack p.c_initial
