(* Types *)
type queue = {
  data: int array;
  size: int ref;
  capacity: int;
}

let new_queue (n: int) = 
  {
    data = (Array.make n 0); 
    size = ref 0; 
    capacity = n
  }

let push (q: queue) (v: int) =
  q.size := (!(q.size) mod q.capacity) + 1;
  q.data.(!(q.size) - 1) <- v

let pop (q: queue) =
  q.size := (!(q.size) - 1 + q.capacity) mod q.capacity;
  q.data.(!(q.size))
  
let peek (q: queue) = 
  let v = pop q in
  push q v;
  v

let push_left_q (q: queue) (v: int) =
  if !(q.size) = 0 then
    push q v
  else
    let tmp = new_queue !(q.size) in
    while !(q.size) > 0 do
      push tmp (pop q)  
    done;
    push q v;
    while !(tmp.size) > 0 do
      push q (pop tmp)
    done

(* ----------------------------------------------------------------------------
Deque
---------------------------------------------------------------------------- *)
type deque = {
  data: int array;
  first: int ref;
  last: int ref;
  capacity: int;
}

let new_deque (n: int) =
  {
    data = (Array.make n 0);
    first = ref 0;
    last = ref 0;
    capacity = n;
  }

let push_right (q: deque) (v: int) =
  q.last := (!(q.last) mod q.capacity) + 1;
  q.data.(!(q.last) - 1) <- v

let push_left (q: deque) (v: int) = 
  q.first := (!(q.first) - 1 + q.capacity) mod q.capacity;
  q.data.(!(q.first)) <- v

let pop_left (q: deque)=
  q.first := (!(q.first) - 1 + q.capacity) mod q.capacity;
  q.data.(!(q.last))

let pop_right (q: deque) =
  q.first := (!(q.first) mod q.capacity) + 1;
  q.data.(!(q.first) - 1)


(* ----------------------------------------------------------------------------
Correction
---------------------------------------------------------------------------- *)
type 'a queue_alt = {
  data: 'a array;
  mutable first: int;
  mutable last: int;
}

(* Comme pour les piles représentées par des tableaux, il n’est pas possible de stocker plus de 𝑛 − 1
valeurs dans un tel tableau : le cas 𝑑𝑒𝑏𝑢𝑡 = 𝑓𝑖𝑛 correspond à une file vide. *)
