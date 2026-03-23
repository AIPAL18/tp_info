type candidat = {
  mutable k : int;
  placement : int array
  }
(* le candidat est représenté par les [k] premières cases de [placement] *)

let a_rejeter c =
  let k = Array.length c in
  let i = ref 0 and b = ref true in
  while !i < k - 1 && !b do
    b := c.(!i) <> c.(k-1) && abs (c.(!i) - c.(k-1)) <> k - 1 - !i;
    (*         ligne      ****                    diagonale *)
    incr i
  done;
  not !b