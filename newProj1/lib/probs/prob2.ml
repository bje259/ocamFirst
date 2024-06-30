(*
# penultimate [ "a" ; "b" ; "c" ; "d" ];;
- : string option = Some "c"
# penultimate [ "a" ];;
- : 'a option = None
# penultimate [];;
- : 'a option = None
*)

let rec penultimate listIn = match listIn with
  | [] -> None
  | [_] -> None
  | [x;_] -> Some x
  | _::u -> penultimate u;;


