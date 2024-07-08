(* debugUtil.ml *)

open Stdlib.Format
open Base
open Types
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
(* let () = *)
(*   let n = 10 in *)
(*   print_bin n (* Output: 0b1010 *) *)

(* Formatter for binary printing *)
let pp_print_bin fmt n = Stdlib.Format.fprintf fmt "0b%s" (int_to_bin n)

(* Example usage with Format.printf *)
(* let () = *)
(*   let n = 10 in *)
(*   Stdlib.Format.printf "%a\n" pp_print_bin n (* Output: 0b1010 *) *)

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
let intPP = Stdlib.Format.pp_print_int
let charPP = Stdlib.Format.pp_print_char
let stringPP = Stdlib.Format.pp_print_string

let flexArr_to_Arr flexArr =
  let n = Flex_array.length flexArr in
  let arr = Array.create ~len:n (Flex_array.get flexArr 0) in
  for i = 0 to n - 1 do
    arr.(i) <- Flex_array.get flexArr i
  done;
  arr

let gridToAry (grid : 'a Agrid.t) =
  let n = Agrid.width grid in
  let m = Agrid.height grid in
  let arr =
    Array.create ~len:n (Array.create ~len:m (Agrid.get grid ~x:0 ~y:0))
  in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      arr.(i).(j) <- Agrid.get grid ~x:i ~y:j
    done
  done;
  arr

let ppGrid grid f = Stdlib.Format.printf "%a@.@." (Agrid.pp f) grid

let printArray : type a. ?f:a transformer -> a array -> unit =
 fun ?(f = Identity) arr ->
  let n = Array.length arr in
  for i = 0 to n - 1 do
    match f with
    | Identity -> Stdlib.Printf.printf "%2s" (Stdlib.Obj.magic arr.(i) : string)
    | Function g -> Stdlib.Printf.printf "%2s" (g arr.(i))
  done;
  Stdlib.Printf.printf "\n"

let printFlex_array ?(f = Identity) arr =
  let n = Flex_array.length arr in
  for i = 0 to n - 1 do
    match f with
    | Identity -> Stdlib.print_endline (Flex_array.get arr i)
    | Function g -> Stdlib.print_endline (g (Flex_array.get arr i))
  done

let printArrays ?(f = Identity) arrs =
  let n = Array.length arrs in
  let m = Array.length arrs.(0) in
  (* let merge = Array.concat (Array.to_list arrs) in *)
  for i = 0 to m - 1 do
    let counter = List.range (n - 1) 0 in
    let rec aux acc = function
      | [] -> acc
      | hd :: tl -> aux (arrs.(i).(hd) :: acc) tl
    in
    let result = aux [] counter in
    let () = debug_print_int_list result in
    match f with
    | Identity -> printArray (Array.of_list result)
    | Function g -> printArray ~f:(Function g) (Array.of_list result)
  done

let printArrays2 ?(f = Identity) arrs =
  let n = Array.length arrs in
  let m = Array.length arrs.(0) in
  (* let merge = Array.concat (Array.to_list arrs) in *)
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      match f with
      | Identity ->
          Stdlib.Printf.printf "%2s" (Stdlib.Obj.magic arrs.(j).(i) : string)
      | Function g -> Stdlib.Printf.printf "%2s" (g arrs.(j).(i))
    done;
    Stdlib.print_newline ()
  done

(* Helper function to create a string of spaces for margin *)
let make_margin n = String.make n ' '

(* Convert a 2D array of integers to a 2D array of strings *)
let int_to_string_array arr = Array.map ~f:(Array.map ~f:Int.to_string) arr

(* Helper function to print error and return an empty array *)
let handle_transpose_error () =
  Stdlib.print_endline "Error: Arrays are not of the same length";
  [||]

(* Function to safely transpose an array and handle errors *)
let safe_transpose arr =
  match Array.transpose arr with
  | None -> handle_transpose_error ()
  | Some transposed_arr -> transposed_arr

(* Function to concatenate two 2D arrays with a margin in between *)
let hstack_with_margin arr1O arr2O margin =
  let arr1N = safe_transpose arr1O in
  let arr2N = safe_transpose arr2O in

  (* Determine the overall dimensions *)
  let rows1 = Array.length arr1N in
  let rows2 = Array.length arr2N in
  let rows = max rows1 rows2 in
  let cols1 =
    if rows1 > 0 then
      Array.length arr1N.(0)
    else
      0
  in
  let cols2 =
    if rows2 > 0 then
      Array.length arr2N.(0)
    else
      0
  in
  let margin_str = String.make margin ' ' in

  (* Create the result array *)
  let result = Array.create ~len:rows "" in

  (* Concatenate rows *)
  for i = 0 to rows - 1 do
    let row1 =
      if i < rows1 then
        arr1N.(i)
      else
        Array.create ~len:cols1 ""
    in
    let row2 =
      if i < rows2 then
        arr2N.(i)
      else
        Array.create ~len:cols2 ""
    in
    result.(i) <-
      String.concat
        [
          String.concat ~sep:" " (Array.to_list row1);
          margin_str;
          String.concat ~sep:" " (Array.to_list row2);
        ]
  done;

  (* Return the result array *)
  result

(* let hstack_with_margin arr1O arr2O margin = *)
(*   let arr1 = Array.transpose arr1O in *)
(*   let arr2 = Array.transpose arr2O in *)
(*   let arr1N = *)
(*     match arr1 with *)
(*     | None -> *)
(*         Stdlib.print_endline "Error: Arrays are not of the same length"; *)
(*         [||] *)
(*     | Some arr1 -> arr1 *)
(*   in *)
(*   let arr2N = *)
(*     match arr2 with *)
(*     | None -> *)
(*         Stdlib.print_endline "Error: Arrays are not of the same length"; *)
(*         [||] *)
(*     | Some arr2 -> arr2 *)
(*   in *)
(*   (* Step 1: Determine the overall dimensions *) *)
(*   let rows1 = Array.length arr1N in *)
(*   let rows2 = Array.length arr2N in *)
(*   let rows = max rows1 rows2 in *)
(*   let cols1 = *)
(*     if rows1 > 0 then *)
(*       Array.length arr1N.(0) *)
(*     else *)
(*       0 *)
(*   in *)
(*   let cols2 = *)
(*     if rows2 > 0 then *)
(*       Array.length arr2N.(0) *)
(*     else *)
(*       0 *)
(*   in *)
(*   let margin_str = make_margin margin in *)
(**)
(*   (* Step 2: Create the result array *) *)
(*   let result = Array.create ~len:rows "" in *)
(**)
(*   (* Step 3: Concatenate rows *) *)
(*   for i = 0 to rows - 1 do *)
(*     let row1 = *)
(*       if i < rows1 then *)
(*         arr1N.(i) *)
(*       else *)
(*         Array.create ~len:cols1 "" *)
(*     in *)
(*     let row2 = *)
(*       if i < rows2 then *)
(*         arr2N.(i) *)
(*       else *)
(*         Array.create ~len:cols2 "" *)
(*     in *)
(*     result.(i) <- *)
(*       String.concat *)
(*         [ *)
(*           String.concat ~sep:" " (Array.to_list row1); *)
(*           margin_str; *)
(*           String.concat ~sep:" " (Array.to_list row2); *)
(*         ] *)
(*   done; *)
(**)
(*   (* Step 4: Return the result array *) *)
(*   result *)

let print_combined_array arr = Array.iter ~f:Stdlib.print_endline arr
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

let printTreeA (tree : 'a tree) f =
  let checkR = function Empty -> true | _ -> false in
  let rec aux prefix is_last = function
    | Empty -> ""
    | Node (l, v, r) ->
        let current =
          Printf.sprintf "%s%s%s\n" prefix
            (if is_last then
               "└── "
             else
               "├── ")
            (f v)
        in
        let new_prefix =
          prefix
          ^
          if is_last then
            "    "
          else
            "│   "
        in
        let left = aux new_prefix (checkR r) l in
        let right = aux new_prefix true r in
        current ^ left ^ right
  in
  Stdlib.print_endline (aux "" true tree)
