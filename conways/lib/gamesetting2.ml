open Notty
open Notty_unix
open Agrid
open Base
open! Ocamlutils.DebugUtil.Printing
open! Ocamlutils.DebugUtil.ConvUtils
open! Ocamlutils.Types
module Types = Ocamlutils.Types
open! NottyConway

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

let get_cell grid x y =
  if x < 0 || y < 0 || x >= Agrid.width grid || y >= Agrid.height grid then
    Dead
  else
    Agrid.get grid ~x ~y

let count_alive_neighbors grid x y =
  let neighbors =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  List.fold_left neighbors ~init:0 ~f:(fun acc (dx, dy) ->
      match get_cell grid (x + dx) (y + dy) with
      | Alive -> acc + 1
      | Dead -> acc)

let next_state grid x y =
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

let update_grid grid = Agrid.mapi (fun ~x ~y _ -> next_state grid y x) grid

(* State types for the game *)
type state = {
  grid : conwayCell Agrid.t;
  running : bool;
  width : int;
  height : int;
  debug_msg : string;
}

let initial_state width height =
  {
    grid = initgrid ~width ~height ();
    running = false;
    width;
    height;
    debug_msg = "";
  }

let rec main_loop term state =
  Term.image term (nottyPPGrid ~debug_msg:state.debug_msg state.grid);
  Unix.sleepf 0.5;
  let state =
    if state.running then
      { state with grid = update_grid state.grid }
    else
      state
  in
  handle_events term state

and handle_events term state =
  match Term.event term with
  | `Key (`ASCII 'q', _) -> Term.release term
  | `Key (`ASCII 'p', _) ->
      main_loop term { state with running = not state.running }
  | `Key (`ASCII 's', _) ->
      main_loop term
        { state with grid = update_grid state.grid; running = false }
  | `Mouse (`Press ((`Left | `Right) as btn), (x, y), _) ->
      let grid =
        Agrid.mapi
          (fun ~x:grid_x ~y:grid_y cell ->
            if grid_x = x / 2 && grid_y = y then
              match cell with Alive -> Dead | Dead -> Alive
            else
              cell)
          state.grid
      in
      let running =
        match btn with `Left -> state.running | `Right -> not state.running
      in
      let debug_msg = Printf.sprintf "Mouse click at (%d, %d)" x y in
      main_loop term { state with grid; running; debug_msg }
  | _ -> main_loop term state

let main () =
  let term = Term.create () in
  let state = initial_state 20 20 in
  main_loop term state

(* let () = main () *)
