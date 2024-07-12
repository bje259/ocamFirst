open Notty
open Notty_unix
open Agrid
open Base
open! Ocamlutils.DebugUtil.Printing
open! Ocamlutils.DebugUtil.ConvUtils
open! Ocamlutils.Types
module Types = Ocamlutils.Types

(* Initialize grid with given width and height *)
let initgrid ?(width = 20) ?(height = 20) () =
  let emptyG =
    Agrid.of_array (Array.make_matrix ~dimx:width ~dimy:height Dead)
  in
  Agrid.map
    (fun _ ->
      if false then
        Alive
      else
        Dead)
    emptyG

(* Safely get the cell, returning Dead if out of bounds *)
let get_cell2 (grid : conwayCell Agrid.t) x y =
  if x < 0 || y < 0 || x >= Agrid.width grid || y >= Agrid.height grid then
    Dead
  else
    Agrid.get grid ~x ~y

let get_cell (grid : conwayCell Agrid.t) x y =
  let width = Agrid.width grid in
  let height = Agrid.height grid in
  let wrapped_x = (x + width) % width in
  let wrapped_y = (y + height) % height in
  Agrid.get grid ~x:wrapped_x ~y:wrapped_y

(* Count alive neighbors around a given cell *)
let count_alive_neighbors grid x y =
  let neighbors =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  List.fold_left neighbors ~init:0 ~f:(fun acc (dx, dy) ->
      match get_cell grid (x + dx) (y + dy) with
      | Alive -> acc + 1
      | Dead -> acc)

(* Apply the rules to a cell *)
let next_state grid y x =
  let alive_neighbors = count_alive_neighbors grid x y in
  match get_cell grid x y with
  | Alive ->
      if alive_neighbors < 2 || alive_neighbors > 3 then
        Dead
      else
        Alive
  | Dead ->
      if alive_neighbors = 3 then
        Alive
      else
        Dead

(* Update the grid *)
let update_grid (grid : conwayCell t) =
  Agrid.mapi (fun ~x ~y _ -> next_state grid x y) grid

let extract_alive_cells (grid : conwayCell t) =
  let width = Agrid.width grid in
  let height = Agrid.height grid in
  let alive_cells = ref [] in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      match Agrid.get grid ~x ~y with
      | Dead -> ()
      | Alive -> alive_cells := (x, y) :: !alive_cells
    done
  done;
  !alive_cells

let place_cells grid cell_coords place_func =
  List.fold_left cell_coords ~init:grid ~f:(fun grid (x, y) ->
      place_func grid x y Alive)

let place_relative_cells grid ~x0 ~y0 rel_coords =
  let place_func grid x y cell = Agrid.set grid ~x:(x0 + x) ~y:(y0 + y) cell in
  place_cells grid rel_coords place_func

let place_absolute_cells grid abs_coords =
  let place_func grid x y cell = Agrid.set grid ~x ~y cell in
  place_cells grid abs_coords place_func
