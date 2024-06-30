
let rec last listIn = match listIn with
  | [] -> None
  | _::y::u -> last (y::u)
  | x::_ -> Some x;;

