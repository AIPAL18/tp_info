open Printf

type ('a, 'b) general_tree = 
  | Leaf of 'b
  | Node of 'a * ('a, 'b) general_tree list

let parcours_largeur (a: ('a, 'b) general_tree) (f_node: 'a -> unit) (f_leaf: 'b -> unit) = 
  let file = Queue.create () in
  Queue.push a file;
  while not (Queue.is_empty file) do
    match Queue.pop file with
    | Leaf e -> f_leaf e
    | Node(e, t) -> 
      f_node e;
      List.iter (fun tree -> Queue.push tree file) t;
  done

let exemple =
  Node (
    'A',
    [
      Node ('B', [ Leaf 1; Leaf 2 ]);
      Node ('C', [ Node ('D', [ Leaf 3; Leaf 4 ]); Leaf 5; Leaf 6 ]);
    ]
  )

let () = printf "{"
let () = parcours_largeur exemple (printf "\n\t%c: ") (printf "%d, ")
let () = printf "\n}"