open Printf
open Main.DictHTC

let d = make 10
let nb = 20
let _ =
  assert(nb > 0)
let _ =
  assert(length d = 0);
  for i = 0 to nb do
    add d i (string_of_int i);
    assert(List.hd(d.data.(d.hash i)) = (i, string_of_int i));
    assert(length d = (i + 1))
  done;
  printf "Success: add\nSuccess: length\n"
let _ =
  for i = 0 to nb do
    assert(mem d i);
  done;
  for i = nb + 1 to (nb*2) do
    assert(not(mem d i))
  done;
  printf "Success: mem\n"
let _ =
  (* w/o exception *)
  for i = 0 to nb do
    assert(find d i = string_of_int i)
  done;
  (* w/ exception *)
  for i = nb + 1 to (nb * 2) do
    try
      ignore(find d i);
      assert(false)
    with e->
      ()
  done;
  printf "Success: find\n"
let _ =
  (* Should be Some *)
  for i = 0 to nb do
    assert(Option.is_some (find_opt d i));
    assert(Option.get (find_opt d i) = string_of_int i)
  done;
  (* Should be None *)
  for i = (nb + 1) to (nb * 2) do
    assert(Option.is_none (find_opt d i))
  done;
  printf "Success: find_opt\n"
let _ =
  (* correctness key-value *)
  iter (fun k v -> assert(string_of_int k = v)) d;
  (* completeness *)
  let visited = Array.make (nb + 1) false in
  iter (fun k v -> (assert(visited.(k) = false); visited.(k) <- true)) d;
  Array.iter (fun e -> assert(e)) visited;
  printf "Success: iter\n"
let _ =
  (* w/o exception *)
  for i = 0 to nb do
    replace d i (string_of_int (nb - 1 - i));
    assert(find d i = (string_of_int (nb - 1 - i)));
    assert(length d = (nb + 1))
  done;
  (* w/ exception *)
  for i = (nb + 1) to (nb * 2) do
    try
      replace d i (string_of_int (nb - 1 - i));
      assert(false)
    with | Not_found -> ()
         | _         -> assert(false)
  done;
  printf "Success: replace\n"
let _ =
  for i = 0 to nb do
    remove d i;
    assert(length d = nb - i);
    assert(Option.is_none (find_opt d i))
  done;
  printf "Success: remove\n"
let _ =
  for i = 0 to nb do
    add d i (string_of_int i);
    add d i "____";
    assert(find d i = "____");
    remove d i;
    assert(find d i = (string_of_int i))
  done;
  printf "Success: add-remove (adding should hide last, hide should reveal last)\n"

open Main.DDictHTC

let size = 10
let d = make size
let _ = 
  for i = 0 to nb do
    add d i (string_of_int i)
  done
let _ = 
  resize d;
  for i = 0 to nb do
    assert(Option.is_some (find_opt d i))
  done;
  assert(Array.length (d.data) = 2 * size);
  printf "Success resize\n"

  