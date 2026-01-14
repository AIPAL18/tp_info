open Printf

let debug = true
let from_bool (b: bool) = if b then "true" else "false"

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

let into (s: string) (c: char) = String.map (fun _ -> c) s

(* Print or assert *)
let pa ((file:string),(lnum: int),(_: int),(_: int)) (expr: bool) =
  if debug then
    if expr then
      (* printf  "%s---- |%s\n" (into file '-')  (from_bool expr)  *)
      ()
    else
      printf "%s:%03d |%s\n" file lnum (from_bool expr) 
  else
    assert(expr)
