(*if first argument is greater than second, produce a list in decreasing order.*)
(* # range 4 9;; *)
(* - : int list = [4; 5; 6; 7; 8; 9] *)
(* # range 9 4;; *)
(* - : int list = [9; 8; 7; 6; 5; 4] *)

let range a b = 
  let dir = if a > b then 1 else -1 in
  let rec aux a b acc = 
    if a = b then b :: acc
    else aux a (b + dir) (b :: acc)
  in aux a b []



