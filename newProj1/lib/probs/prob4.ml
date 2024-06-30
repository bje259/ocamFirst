let rec dupCreate = function
  | [] -> []
  | h::t -> h::h::(dupCreate t);;

let rec dupCreate2 acc = function
  | [] -> acc
  | h::t -> dupCreate2 (acc @ (h::h::[])) t;;

let outList listIn = List.fold_left (fun acc h -> acc @ (h :: h :: [])) [] listIn;;

let outList2 listIn = let sList = (List.sort compare listIn) in List.merge compare sList sList;;
(*(fun h -> (let result = h :: h :: [] in let () = Printf.printf "%s\n" (Debug.string_of_string_list(result)) in result)) listIn;;*)
