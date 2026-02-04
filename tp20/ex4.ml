(*
3 14 + 5 2 − ×

(3 + 14) × (5 - 2)

= 17×3
= 51
*)
open Printf

type operateur = Plus | Moins | Mult | Div

type expr_postfixe =
| Val of int
| Op of operateur

exception Unvalid_Expression of string

(* evalue_postfixe : expr_element list -> int *)
let rec evalue_postfixe (expr: expr_postfixe list) = 
    let (s: int Stack.t) = Stack.create ()
  in
    let rec aux (e: expr_postfixe list) =
      match e with
      | [] ->
        (
        match Stack.pop_opt s with
        | None -> raise (Unvalid_Expression "No result (empty expression)")
        | Some v -> v
        )
      | (Val v)::q -> Stack.push v s; aux q
      | (Op op)::q -> 
        let f = match op with
          | Plus -> ( + )
          | Moins -> ( - )
          | Mult -> ( * )
          | Div -> ( / )
        in
        let v1 = Stack.pop_opt s in
        let v2 = Stack.pop_opt s in 
          let v1', v2' = match v1, v2 with
          | None, None | None, _ | _, None -> raise (Unvalid_Expression "Not enough numbers")
          | Some a, Some b -> a, b
        in Stack.push (f v2' v1') s; aux q
  in
    aux expr

    (* 3 14 + 5 2 − × *)
    
let () = printf "%d\n" (evalue_postfixe [Val(3); Val(14); Op(Plus); Val(5); Val(2); Op(Moins); Op(Mult)])

type expr_infixe =
| V of int
| O of operateur
| Par_ouvr
| Par_ferm

let check_para_UNOPTI (expr: expr_infixe list) = 
    let queue = Queue.create ()
  in
    let rec aux (e: expr_infixe list) = 
      match e with
      | [] -> Queue.is_empty queue
      | Par_ouvr::q -> Queue.push Par_ouvr queue; aux q
      | Par_ferm::q  when Queue.is_empty queue -> false
      | Par_ferm::q -> ignore(Queue.pop queue); aux q
      | _::q -> aux q
  in
    aux expr

let check_para (expr: expr_infixe list) = 
    let count = ref 0
  in
    let rec aux (e: expr_infixe list) = 
      match e with
      | [] -> !count = 0
      | Par_ouvr::q -> count := (!count + 1); aux q
      | Par_ferm::q  when !count = 0 -> false
      | Par_ferm::q -> count := (!count -1) ; aux q
      | _::q -> aux q
  in
    aux expr

(* let get_line (file: string) (lnum: int) =
    let (stream: in_channel) = open_in file
  in
    let rec aux (n: int) = 
      assert(n >= 0);
      if n > 0 then 
      begin
        ignore(input_line stream); aux (n - 1)
      end
      else
        input_line stream
  in
    aux (lnum - 1)

let rec get_after sub str =
  if sub = "" || str = "" then
    str
  else
      let n_str = Extstring.trim str
    in
      if String.starts_with ~prefix:sub n_str then
        n_str
      else
        get_after sub n_str

let eval (((file: string), (lnum: int), (cnum: int), (enum: int)), (res: 'a))   = 
    let (line: string) = get_line (file) lnum
  in
    let after = get_after "__POS_OF__" line
  in
    after
    (* let (len: int) = after;
  in
    String.sub line cnum len *)
let _ = printf "check_para %s\n" (eval (__POS_OF__(evalue_postfixe [Val(3); Val(14); Op(Plus); Val(5); Val(2); Op(Moins); Op(Mult)]))) *)

