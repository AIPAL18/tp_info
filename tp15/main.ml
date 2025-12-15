(*Exercice 1*)

type 'a arbre =
    | V
    | N of 'a * 'a arbre * 'a arbre

let ex = N(
    0, 
    N(4, 
        N(5, V, V), 
        N(3, 
            N(1, V, V), 
            N(6, V, V)
        )
    ), 
    N(2, 
        N(4, V, V), 
        V
    )
)

let rec prefixe_inefficace (a: 'a arbre) = 
    match a with
    | V -> []
    | N(e, g, d) -> [e] @ (prefixe_inefficace g) @ (prefixe_inefficace d)

let rec infixe_inefficace (a: 'a arbre) = 
    match a with
    | V -> []
    | N(e, g, d) -> (infixe_inefficace g) @ [e] @ (infixe_inefficace d)

let rec postfixe_inefficace (a: 'a arbre) = 
    match a with
    | V -> []
    | N(e, g, d) -> (postfixe_inefficace g) @ (postfixe_inefficace d) @ [e]

let rec print_list (l: 'a list) (print_e: 'a -> unit) =
    match l with
    | [] -> print_newline ()
    | h::tl -> print_e h ; print_string ", " ; print_list tl print_e

let () = print_list (prefixe_inefficace ex) print_int
let () = print_list (infixe_inefficace ex) print_int
let () = print_list (postfixe_inefficace ex) print_int

