(* debugUtil.ml *)

open Stdlib.Format
open Base
(* open Agrid *)

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
  "[" ^ String.concat ~sep:"; " (List.map lst ~f:Int.to_string) ^ "]"

let string_of_float_list lst =
  "[" ^ String.concat ~sep:"; " (List.map lst ~f:Float.to_string) ^ "]"

let string_of_string_list lst =
  "["
  ^ String.concat ~sep:"; " (List.map lst ~f:(fun s -> "\"" ^ s ^ "\""))
  ^ "]"

let string_of_list f lst = "[" ^ String.concat ~sep:"; " (List.map lst ~f) ^ "]"
let debug_print_list f lst = Stdlib.print_endline (string_of_list f lst)
let debug_print_int_list lst = Stdlib.print_endline (string_of_int_list lst)
let debug_print_float_list lst = Stdlib.print_endline (string_of_float_list lst)

let debug_print_string_list lst =
  Stdlib.print_endline (string_of_string_list lst)

let debug_print msg = Stdlib.print_endline ("DEBUG: " ^ msg)
let string_of_tuple f1 f2 (x1, x2) = "(" ^ f1 x1 ^ ", " ^ f2 x2 ^ ")"

let string_of_tuple3 f1 f2 f3 (x1, x2, x3) =
  "(" ^ f1 x1 ^ ", " ^ f2 x2 ^ ", " ^ f3 x3 ^ ")"

(* Define string conversion functions *)
let string_of_char_option = function
  | Some c -> String.make 1 c
  | None -> "None"

let pp_list pp_elem fmt lst =
  fprintf fmt "[";
  let rec pp_elems = function
    | [] -> ()
    | [ x ] -> pp_elem fmt x
    | x :: xs ->
        pp_elem fmt x;
        fprintf fmt "; ";
        pp_elems xs
  in
  pp_elems lst;
  fprintf fmt "]"

let pp_tuple pp_fst pp_snd fmt (x1, x2) =
  fprintf fmt "(%a, %a)" pp_fst x1 pp_snd x2

let pp_tuple3 pp_fst pp_snd pp_trd fmt (x1, x2, x3) =
  fprintf fmt "(%a, %a, %a)" pp_fst x1 pp_snd x2 pp_trd x3

(* Function to convert an integer to a binary string *)
let int_to_bin n =
  let rec aux n =
    if n = 0 then
      ""
    else
      aux (n lsr 1) ^ Int.to_string (n land 1)
  in
  if n = 0 then
    "0"
  else
    aux n

(* Function to print an integer in binary format *)
let print_bin n = Stdlib.Printf.printf "0b%s\n" (int_to_bin n)

(* Example usage *)
let () =
  let n = 10 in
  print_bin n (* Output: 0b1010 *)

(* Formatter for binary printing *)
let pp_print_bin fmt n = Stdlib.Format.fprintf fmt "0b%s" (int_to_bin n)

(* Example usage with Format.printf *)
let () =
  let n = 10 in
  Stdlib.Format.printf "%a\n" pp_print_bin n (* Output: 0b1010 *)

(* Example usage *)

(* let () = *)
(*   let pp_int fmt i = fprintf fmt "%d" i in *)
(*   let pp_string fmt s = fprintf fmt "%s" s in *)
(**)
(*   let lst = [ 1; 2; 3; 4 ] in *)
(*   let tup = (1, "two") in *)
(*   let tup3 = (1, "two", 3.0) in *)
(**)
(*   printf "%a\n" (pp_list pp_int) lst; *)
(*   printf "%a\n" (pp_tuple pp_int pp_string) tup; *)
(*   printf "%a\n" (pp_tuple3 pp_int pp_string Stdlib.Format.pp_print_float) tup3 *)

let flexArr_to_Arr flexArr =
  let n = Flex_array.length flexArr in
  let arr = Array.create ~len:n (Flex_array.get flexArr 0) in
  for i = 0 to n - 1 do
    arr.(i) <- Flex_array.get flexArr i
  done;
  arr

let gridToAry grid =
  let n = Agrid.width grid in
  let m = Agrid.height grid in
  let arr = Array.create ~len:n (Array.create ~len:m (Agrid.get grid 0 0)) in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      arr.(i).(j) <- Agrid.get grid i j
    done
  done;
  arr

let ppGrid grid f = Stdlib.Format.printf "%a@.@." (Agrid.pp f) grid

let printArray ?(f = Fn.id) arr =
  let n = Array.length arr in
  for i = 0 to n - 1 do
    Stdlib.print_endline (f arr.(i))
  done

let printFlex_array ?(f = Fn.id) arr =
  let n = Flex_array.length arr in
  for i = 0 to n - 1 do
    Stdlib.print_endline (f (Flex_array.get arr i))
  done

let printArrays ?(f = Fn.id) arrs =
  let n = Array.length arrs in
  (* let merge = Array.concat (Array.to_list arrs) in *)
  for i = 0 to n - 1 do
    printArray ~f arrs.(i)
  done
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

let string_of_char c = String.make 1 c

let string_of_char_list lst =
  "[" ^ String.concat ~sep:"; " (List.map lst ~f:string_of_char) ^ "]"

let string_of_chars lst = String.concat ~sep:"" (List.map lst ~f:string_of_char)

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
