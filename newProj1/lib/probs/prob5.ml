(*# insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;*)
(*- : string list = ["a"; "alfa"; "b"; "c"; "d"]*)

let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t;;

let rec insert_at strAdd n = function
    | [] -> []
    | h :: t -> if n = 0 then (h::strAdd::t) else h :: insert_at strAdd (n - 1) t;;


let rec insert_at2 strAdd n = function
    | [] -> []
    | h :: t -> if n = 0 then (strAdd::h::t) else h :: insert_at2 strAdd (n - 1) t;;
