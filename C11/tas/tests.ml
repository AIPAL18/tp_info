open Main.Tas
open Printf

let t1 = create 10 0

let _ = 
  add 1 t1;
  add 2 t1;
  add 3 t1;
  add 4 t1;
  add 5 t1;
  add 6 t1;
  add 7 t1;
  add 8 t1;
  add 9 t1;
  add 10 t1

let _ =
  let lev = ref 0 in
  for i = 0 to t1.size - 1 do
    if i+1 >= (1 lsl !lev) && i+1 <= ((1 lsl ((!lev)+1)) - 1) then
      ()
    else
      begin
      printf "\n";
      lev := !lev + 1
      end;
    printf "%d, " t1.data.(i)
  done
