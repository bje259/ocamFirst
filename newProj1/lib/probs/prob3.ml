(*# length ["a"; "b"; "c"];;*)
(*- : int = 3*)
(*# length [];;*)
(*- : int = 0*)

let rec len inList = match inList with
  | [] -> 0
  | _::t -> 1 + len t


let rec recFb acc = function
| [] -> fst(acc)
(*| 1::t | 2::t -> test (1,1) t*)
| _::t -> let calc = let first = fst(acc) in let sec = snd(acc) in let () = Printf.printf "IterVal: %d\n" first in recFb (sec,(first+sec)) t in calc;;

let fibb n = recFb (0,1) (List.init (n-1) (fun x -> x));;
(*# fibb 10;;*)
(*IterVal: 1*)
(*IterVal: 1*)
(*IterVal: 2*)
(*IterVal: 3*)
(*IterVal: 5*)
(*IterVal: 8*)
(*IterVal: 13*)
(*IterVal: 21*)
(*IterVal: 34*)
(*IterVal: 55*)
(*- : int = 55*)
