open Printf

(* ----------------------------------------------------------------------------
EXERCICE I.1
---------------------------------------------------------------------------- *)
module DictListAssoc = struct
  type ('a, 'b) t = ('a * 'b) list
  
  exception KeyError

  (* O(1) *)
  let make () = []

  (* O(n) *)
  let rec mem (dict: ('a, 'b) t) (k: 'a) = 
    match dict with
    | [] -> false
    | (k', v)::q when k = k' -> true
    | _::q -> mem q k

  (* O(n) *)
  let rec find (dict: ('a, 'b) t) (k: 'a) = 
    match dict with
    | [] -> raise KeyError
    | (k', v)::q when k = k' -> v
    | _::q -> find q k

  (* O(n) *)
  let rec find_opt (dict: ('a, 'b) t) (k: 'a) = 
    match dict with
    | [] -> None
    | (k', v)::q when k = k' -> Some v
    | _::q -> find_opt q k
  
  (* O(1) *)
  let add (dict: ('a, 'b) t) (k: 'a) (v: 'b) = 
    (k, v)::dict
  
  (* O(n) *)
  let rec remove (dict: ('a, 'b) t) (k: 'a) = 
    match dict with
    | [] -> raise KeyError
    | (k', v)::q when k = k' -> dict
    | e::q -> e::(remove dict k)
end

(* ----------------------------------------------------------------------------
EXERCICE I.2
---------------------------------------------------------------------------- *)
module DictABR = struct
  type ('a, 'b) t =
  | Empty
  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t
  
  exception KeyError

  let make () = Empty

  let rec mem (dict: ('a, 'b) t) (k: 'a) =
    match dict with
    | Empty -> false
    | Node(l, k', v', r) ->
      if k = k' then
        true
      else if k < k' then
        mem l k
      else
        mem r k

  let rec find (dict: ('a, 'b) t) (k: 'a) =
    match dict with
    | Empty -> raise KeyError
    | Node(l, k', v', r) ->
      if k = k' then
        v'
      else if k < k' then
        find l k
      else
        find r k

  let rec find_opt (dict: ('a, 'b) t) (k: 'a) =
    match dict with
    | Empty -> None
    | Node(l, k', v', r) ->
      if k = k' then
        Some v'
      else if k < k' then
        find_opt l k
      else
        find_opt r k
  
  let rec add (dict: ('a, 'b) t) (k: 'a) (v: 'b) = 
    match dict with
    | Empty -> Node(Empty, k, v, Empty)
    | Node(l, k', _, r) when k = k' -> Node(l, k, v, r)
    | Node(l, k', v', r) -> 
      if k < k' then
        Node((add l k v), k', v', r)
      else
        Node(l, k', v', (add r k v))
  
  (* TP16 *)
  let rec remove_min (dict: ('a, 'b) t) = 
    match dict with
    | Empty -> raise KeyError
    | Node(Empty, k, v, r) -> k, v, r
    | Node(l, k, v, r) -> let k', v', l' = remove_min l in
        k', v', Node(l', k, v, r)

  let rec remove (dict: ('a, 'b) t) (key: 'a) = 
      match dict with
      | Empty -> raise KeyError
      | Node(Empty, k, v, r) when k = key -> r
      | Node(l, k, v, Empty) when k = key -> l
      | Node(l, k, v, r) when k < key -> Node(l, k, v, remove r key)
      | Node(l, k, v, r) when k > key -> Node(remove l key, k, v, r)
      | Node(l, k, v, r)              -> 
        let k', v', r' = remove_min r in 
        Node(l, k', v', r')

end
type ('a, 'b) dictABR = ('a, 'b) DictABR.t

let (exDictABR: ('a, 'b) dictABR) = DictABR.make ()
let exDictABR2 = DictABR.add exDictABR 2 "coucou"
let exDictABR3 = DictABR.add exDictABR2 3 "cacao"
let exDictABR4 = DictABR.add exDictABR3 1 "champagne"

(* ----------------------------------------------------------------------------
EXERCICE II.3
---------------------------------------------------------------------------- *)

(* hash_int : int -> int -> int *)
let hash_int (k: int) (m: int) = k mod m

(* hash_string : int -> string -> int *)
let hash_string (m: int) (s: string) =
  let powMod (x: int) (n: int) (m: int) =
    let res = ref 1
  in
    for i = 0 to n - 1 do
      res := (!res * x) mod m
    done;
    !res
  in
    let v = ref 0
  in
    for i = 0 to String.length s - 1 do
      v := (!v + (Char.code s.[i]) * (powMod 256 i m)) mod m
    done;
    !v

let hash_string (m: int) (s: string) = 
    let res = ref 0
  in
    for i = String.length s - 1 downto 0 do
      res := (!res * 256 + Char.code s.[i]) mod m
    done;
    !res

let hash m x = Hashtbl.hash x mod m

(* ----------------------------------------------------------------------------
EXERCICE II.4
---------------------------------------------------------------------------- *)
module DictHTC = struct (* Dict with Hashing Tables by Chaining *)
  type ('a, 'b) t = {
    mutable data : ('a * 'b) list array;
    mutable size : int;
    mutable hash : 'a -> int
  }
  
  exception KeyError

  let make (size: int) = 
    {
      data = Array.make size [];
      size = 0;
      hash = hash size;
    }
  
  let length (dict: ('a, 'b) t) = 
    (* let len = ref 0 in
    Array.iter (fun l -> len := !len + List.length l) dict.data *)
    dict.size
    
  
  let mem (dict: ('a, 'b) t) (k: 'a) = 
    let rec is_in_list (l: ('a * 'b) list) = 
      match l with
      | [] -> false
      | (k', v')::q -> k = k' || is_in_list q
    in
    is_in_list dict.data.(dict.hash k)

  let find (dict: ('a, 'b) t) (k: 'a) =
    let rec get_in_list (l: ('a * 'b) list) = 
      match l with
      | [] -> raise KeyError
      | (k', v')::q -> 
        if k = k' then
          v' 
        else
          get_in_list q
    in
    get_in_list dict.data.(dict.hash k)

  let find_opt (dict: ('a, 'b) t) (k: 'a) =
    let rec get_in_list (l: ('a * 'b) list) = 
      match l with
      | [] -> None
      | (k', v')::q -> 
        if k = k' then
          Some v' 
        else
          get_in_list q
    in
    get_in_list dict.data.(dict.hash k)
  
  let add (dict: ('a, 'b) t) (k: 'a) (v: 'b) =
    dict.data.(dict.hash k) <- (k, v)::dict.data.(dict.hash k);
    dict.size <- dict.size + 1
  
  let remove (dict: ('a, 'b) t) (k: 'a) =
    let rec remove_in_list (l: ('a * 'b) list) = 
      match l with
      | [] -> raise KeyError
      | ((k', v') as t)::q -> 
        if k = k' then
          q
        else
          t::remove_in_list q
    in
    dict.data.(dict.hash k) <- remove_in_list dict.data.(dict.hash k);
    dict.size <- dict.size - 1

  (* replace : ('a, 'b) hash_table -> 'a -> 'b -> unit *)
  let replace (dict: ('a, 'b) t) (k: 'a) (v: 'b) = 
    let rec replace_in_list (l: ('a * 'b) list) = 
      match l with
      | [] -> raise Not_found
      | ((k', v') as t)::q ->
        if k = k' then
          (k, v)::q
        else
          t::replace_in_list q
    in
    dict.data.(dict.hash k) <- replace_in_list dict.data.(dict.hash k)
  
  let iter (f: 'a -> 'b -> unit) (dict: ('a, 'b) t) =
    Array.iter (List.iter (fun (k, v) -> f k v)) dict.data 

end
type ('a, 'b) hash_table = ('a, 'b) DictHTC.t 

(* ----------------------------------------------------------------------------
II.5
---------------------------------------------------------------------------- *)

module DDictHTC = struct (* Dict with Hashing Tables by Chaining *)
  type ('a, 'b) t = {
    mutable data : ('a * 'b) list array;
    mutable size : int;
    mutable hash : 'a -> int
  }
  
  exception KeyError

  let make (size: int) = 
    {
      data = Array.make size [];
      size = 0;
      hash = hash size;
    }
  
  let length (dict: ('a, 'b) t) = 
    (* let len = ref 0 in
    Array.iter (fun l -> len := !len + List.length l) dict.data *)
    dict.size
    
  
  let mem (dict: ('a, 'b) t) (k: 'a) = 
    let rec is_in_list (l: ('a * 'b) list) = (* is_in_list vérifie si k est dans l *)
      match l with
      | [] -> false
      | (k', v')::q -> k = k' || is_in_list q
    in
    is_in_list dict.data.(dict.hash k)

  let find (dict: ('a, 'b) t) (k: 'a) =
    let rec get_in_list (l: ('a * 'b) list) = 
      match l with
      | [] -> raise KeyError
      | (k', v')::q -> 
        if k = k' then
          v' 
        else
          get_in_list q
    in
    get_in_list dict.data.(dict.hash k)

  let find_opt (dict: ('a, 'b) t) (k: 'a) =
    let rec get_in_list (l: ('a * 'b) list) = 
      match l with
      | [] -> None
      | (k', v')::q -> 
        if k = k' then
          Some v' 
        else
          get_in_list q
    in
    get_in_list dict.data.(dict.hash k)
  
  let add (dict: ('a, 'b) t) (k: 'a) (v: 'b) =
    dict.data.(dict.hash k) <- (k, v)::dict.data.(dict.hash k);
    dict.size <- dict.size + 1
  
  let remove (dict: ('a, 'b) t) (k: 'a) =
    let rec remove_in_list (l: ('a * 'b) list) = 
      match l with
      | [] -> raise KeyError
      | ((k', v') as t)::q -> 
        if k = k' then
          q
        else
          t::remove_in_list q
    in
    dict.data.(dict.hash k) <- remove_in_list dict.data.(dict.hash k);
    dict.size <- dict.size - 1

  (* replace : ('a, 'b) hash_table -> 'a -> 'b -> unit *)
  let replace (dict: ('a, 'b) t) (k: 'a) (v: 'b) = 
    let rec replace_in_list (l: ('a * 'b) list) = 
      match l with
      | [] -> raise Not_found
      | ((k', v') as t)::q ->
        if k = k' then
          (k, v)::q
        else
          t::replace_in_list q
    in
    dict.data.(dict.hash k) <- replace_in_list dict.data.(dict.hash k)
  
  let iter (f: 'a -> 'b -> unit) (dict: ('a, 'b) t) =
    Array.iter (List.iter (fun (k, v) -> f k v)) dict.data 
  
  let resize (dict: ('a, 'b) t) = (* Attention à l'ordre les clés qui sont caché par les nouvelles *)
      let new_dict = make ((Array.length dict.data) * 2)
    in
      iter (fun k v -> add new_dict k v) dict;
      dict.data <- new_dict.data;
      dict.hash <- new_dict.hash;
      dict.size <- new_dict.size

end
type ('a, 'b) dynamic_hash_table = ('a, 'b) DDictHTC.t 
