(*open Debug;;*)
open Types;;

let at_levelN (tr, lev) = 
    let rec at_leveln_aux (tr, lev, acc) = match tr with
      | Nleaf ->  acc
      | Ntree (v, _ ) when lev = 0 -> v::acc
      | Ntree (_, children) -> List.fold_right (fun child acc -> at_leveln_aux (child, lev-1, acc)) children acc
      in at_leveln_aux (tr, lev, []);;

let countN (tr) = 
    let rec countn_aux (tr, acc) = match tr with
      | Nleaf ->  acc
      | Ntree (_, children) -> List.fold_right (fun child acc -> countn_aux (child, acc)) children (acc+1)
      in countn_aux (tr, 0);;

let iplN (tr) = 
    let rec countn_aux (tr, depth, acc) = match tr with
      | Nleaf ->  let () = print_endline (String.concat "," [string_of_int depth; string_of_int acc;]) in acc
      | Ntree (_, []) -> let () = print_endline (String.concat "," [string_of_int depth; string_of_int acc;]) in acc
      | Ntree (_, children) -> List.fold_left (fun acc child -> countn_aux (child,depth+1, acc)) (acc+1*depth) children
    in countn_aux (tr, 0, 0);;
