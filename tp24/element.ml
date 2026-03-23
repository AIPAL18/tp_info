type 'a element = Empty | Element of 'a

let is_element (e: 'a element): bool = 
  match e with
  | Element(_) -> true
  | _ -> false

let get_element (e: 'a element): 'a = 
  match e with
  | Element(x) -> x
  | Empty -> invalid_arg "element is Empty"

