open Agrid

(* open Flex_array *)
open! Base
(* open Container *)

(* open DebugUtil.Printing *)
(* open DebugUtil.ConvUtils *)
open! Ocamlutils.DebugUtil.Printing
open! Ocamlutils.DebugUtil.ConvUtils
open! Ocamlutils.Types
module Types = Ocamlutils.Types

(* Print the grid *)
let print_grid2 grid =
  Agrid.iter
    (fun cell ->
      match cell with Alive -> print_string "██" | Dead -> print_string "  ")
    grid;
  print_newline ()

let string_of_cell cell = match cell with Alive -> "██" | Dead -> "  "

let pp_cell (fmt : Stdlib.Format.formatter) n =
  Stdlib.Format.fprintf fmt "%s" (string_of_cell n)

let ppCol fmt () = Stdlib.Format.pp_print_char fmt '|'
let ppRow fmt () = Stdlib.Format.pp_print_string fmt "|\n|"

let ppGrid2 grid f =
  Stdlib.Format.printf "|%a|@.@."
    (Agrid.pp ~pp_sep_row:ppRow ~pp_sep_col:ppCol f)
    grid

let print_grid grid = ppGrid2 grid pp_cell
