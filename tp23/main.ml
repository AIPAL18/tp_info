open Printf

type braun = 
  | V
  | N of braun * braun


let rec size_adb (a: braun): int = 
  match a with
  | V -> 0
  | N(g, d) -> 1 + size_adb g + size_adb d

let rec next_adb (a: braun): braun =
  match a with
  | V -> N(V, V)
  | N(g, d) ->
    let size_g = size_adb g in
    let size_d = size_adb d in
    if size_g = size_d then
      N(next_adb g, d)
    else if size_g = size_d + 1 then
      N(g, next_adb d)
    else failwith "next_adb prend un arbre de Braun en entrée!"


let display_adb (a: braun): unit = 
  let lev = ref 0 in
  for i = 0 to (size_adb a) - 1 do
    if i+1 >= (1 lsl !lev) && i+1 <= ((1 lsl ((!lev)+1)) - 1) then
      ()
    else
      begin
      printf "\n";
      lev := !lev + 1
      end;
    printf "*, "
  done;
  printf "\n"

    
let adb0 = V
let adb1 = next_adb adb0
let adb2 = next_adb adb1
let adb3 = next_adb adb2
let adb4 = next_adb adb3
let _ = display_adb (adb4); print_newline ()
let adb5 = next_adb adb4
let adb6 = next_adb adb5
let adb7 = next_adb adb6
let _ = display_adb (adb7)
