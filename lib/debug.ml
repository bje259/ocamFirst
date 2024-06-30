(* debug.ml *)
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
