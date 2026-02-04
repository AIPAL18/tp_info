type 'a file = {
  mutable head: 'a list;
  mutable size: int;
  capacity: int;
}

let new_file (n: int) = 
  {
    head = [];
    size = 0;
    capacity = n;
  }

let push (f: 'a file) (v: 'a) = 
  if f.size < f.capacity then 
  begin
    f.head <- v::f.head;
    f.size <- (f.size + 1); 
  end
  else 
    failwith "overflow"

let pop (f: 'a file) = 
  let rec aux (l: 'a list) (n: 'a list) = 
    match l with
    | [] -> failwith "empty"
    | t::[] -> t, n
    | t::q -> aux q (t::n)
  in
  let r, l = aux f.head [] in
  f.head <- l;
  r
