(* debugUtil.ml *)

(*type nodeRec = {*)
(*  character: char;*)
(*  frequency: int ;*)
(*  left: nodeRec option;*)
(*  right: nodeRec option;*)
(*}*)
(**)
(*type nodeRecOption = nodeRec option*)

(*type 'a pqTree =*)
(*  | Leaf*)
(*  | PqItem of 'a * int*)
(*  | PqNode of ('a * 'a pqTree * 'a pqTree) * int;;*)


(*type 'a tree =*)
(*  | Leaf*)
(*  | Node of 'a * 'a tree * 'a tree;;*)

let string_of_int_list lst =
  "[" ^ (String.concat "; " (List.map string_of_int lst)) ^ "]"

let string_of_float_list lst =
  "[" ^ (String.concat "; " (List.map string_of_float lst)) ^ "]"

let string_of_string_list lst =
  "[" ^ (String.concat "; " (List.map (fun s -> "\"" ^ s ^ "\"") lst)) ^ "]"

let string_of_list f lst =
  "[" ^ (String.concat "; " (List.map f lst)) ^ "]"

let debug_print_list f lst =
  print_endline (string_of_list f lst)

let debug_print_int_list lst =
  print_endline (string_of_int_list lst)

let debug_print_float_list lst =
  print_endline (string_of_float_list lst)

let debug_print_string_list lst =
  print_endline (string_of_string_list lst)

let debug_print msg =
  print_endline ("DEBUG: " ^ msg)

let string_of_tuple f1 f2 (x1, x2) =
  "(" ^ f1 x1 ^ ", " ^ f2 x2 ^ ")"
  let string_of_tuple3 f1 f2 f3 (x1, x2, x3) =
    "(" ^ f1 x1 ^ ", " ^ f2 x2 ^ ", " ^ f3 x3 ^ ")"

(* Define string conversion functions *)
let string_of_char_option = function
  | Some c -> String.make 1 c
  | None -> "None"


(*let string_of_nested_string_list lst : nestedStringListelement = *)
(*  let acc = [] in*)
(*  let strt = "[" in*)
(*  let rec aux acc = function*)
(*    | Str s -> s :: acc*)
(*    | List [] -> "["::acc*)
(*    | List ( sbLst ) :: List t -> aux (aux (strt::h::acc) t) sbLst*)
(**)
(*    (*| List ( h :: t1 ) :: t2 -> aux (aux (strt::h::acc) t1) t2 in*)*)
(*  let res = aux acc lst in*)
(*    string_of_string_list res*)

  (*"[" ^ (String.concat "; " (string_of_string_list res)) ^ "]"*)


(*let string_of_nested_string_list (lst : nestedStringListelement) =*)
(*  let rec aux = function*)
(*    | Str s -> s*)
(*    | List sublst -> "[" ^ (String.concat "; " (List.map aux sublst)) ^ "]"*)
(*  in*)
(*  aux lst*)


(*let debug_print_nested_string_list lst =*)
  (*print_endline (string_of_nested_string_list lst)*)


let string_of_char c = String.make 1 c;;

let string_of_char_list lst =
  "[" ^ (String.concat "; " (List.map string_of_char lst)) ^ "]"

let string_of_chars lst = String.concat "" (List.map string_of_char lst)


(*let printTreeCharN tree =*)
(*      let rec aux prefix is_last = function*)
(*        | Nleaf -> ""*)
(*        | Ntree (v, lst) ->*)
(*            let current = Printf.sprintf "%s%s%s\n" prefix (if is_last then "└── " else "├── ") (string_of_char v) in*)
(*            let new_prefix = prefix ^ (if is_last then "    " else "│   ") in*)
(*            (*let left = aux new_prefix (r=Leaf) l in*)*)
(*            let right = aux new_prefix true r in*)
(*            current ^ left ^ right*)
(*        in print_endline (aux "" true tree)*)
(**)



    (*let printTreeA tree f =*)
    (*  let rec aux prefix is_last = function*)
    (*    | Leaf -> ""*)
    (*    | Node (v, l, r) ->*)
    (*        let current = Printf.sprintf "%s%s%s\n" prefix (if is_last then "└── " else "├── ") (f v) in*)
    (*        let new_prefix = prefix ^ (if is_last then "    " else "│   ") in*)
    (*        let left = aux new_prefix (r=Leaf) l in*)
    (*        let right = aux new_prefix true r in*)
    (*        current ^ left ^ right*)
    (*    in print_endline (aux "" true tree)*)
