module Tas = struct
  type 'p t = {
    data: 'p array;
    mutable size: int;
  }
  
  let create (size: int) (default: 'p): 'p t =
    {
      data = Array.make size default;
      size = 0;
    }
  
  let is_empty (tas: 'p t): bool = 
    tas.size = 0
  
  let is_full (tas: 'p t): bool = 
    tas.size == Array.length tas.data
  
  let add (v: 'p) (tas: 'p t): unit = 
    if is_full tas then
      raise (Invalid_argument "Overflow: the stack is already full")
    else
      tas.data.(tas.size) <- v;
      let indice = ref tas.size in
      while !indice <> 0 && tas.data.((!indice - 1) / 2) < tas.data.(!indice) do
        let prev = tas.data.((!indice - 1) / 2) in
        let curr = tas.data.(!indice) in
        tas.data.((!indice - 1) / 2) <- curr;
        tas.data.(!indice) <- prev
      done;
      tas.size <- tas.size + 1;
      
end

type 'p tas = 'p Tas.t
