open Notty
open Notty_unix
open Agrid
open Base
open! Ocamlutils.DebugUtil.Printing
open! Ocamlutils.DebugUtil.ConvUtils
open! Ocamlutils.Types
module Types = Ocamlutils.Types
open! NtyConway
open Conwaycore

type state = {
  grid : conwayCell Agrid.t;
  running : bool;
  width : int;
  height : int;
  debug_msg : string;
  input_buffer : string;
}

let place_eater grid x y =
  let eater_coords =
    [ (0, 1); (0, 2); (1, 0); (1, 2); (2, 2); (3, 2); (3, 3) ]
  in
  List.fold_left eater_coords ~init:grid ~f:(fun grid (dy, dx) ->
      Agrid.set grid ~x:(x + dx) ~y:(y + dy) Alive)

let place_abs_eater grid =
  let eater_abs_coords =
    [ (33, 33); (32, 33); (32, 32); (32, 31); (32, 30); (31, 30); (30, 31) ]
  in
  List.fold_left eater_abs_coords ~init:grid ~f:(fun grid (x, y) ->
      Agrid.set grid ~x ~y Alive)

let place_rel_glider grid ~x0 ~y0 =
  let glider_rel_coords = [ (1, 0); (2, 1); (0, 2); (1, 2); (2, 2) ] in
  Conwaycore.place_relative_cells grid ~x0 ~y0 glider_rel_coords

let place_abs_glider grid =
  let glider_abs_coords = [ (2, 3); (2, 2); (2, 1); (1, 3); (0, 2) ] in
  Conwaycore.place_absolute_cells grid glider_abs_coords

let place_rel_gliGun grid ~x0 ~y0 =
  let glider_rel_coords =
    [
      (24, 0);
      (22, 1);
      (24, 1);
      (12, 2);
      (13, 2);
      (20, 2);
      (21, 2);
      (34, 2);
      (35, 2);
      (11, 3);
      (15, 3);
      (20, 3);
      (21, 3);
      (34, 3);
      (35, 3);
      (0, 4);
      (1, 4);
      (10, 4);
      (16, 4);
      (20, 4);
      (21, 4);
      (0, 5);
      (1, 5);
      (10, 5);
      (14, 5);
      (16, 5);
      (17, 5);
      (22, 5);
      (24, 5);
      (10, 6);
      (16, 6);
      (24, 6);
      (11, 7);
      (15, 7);
      (12, 8);
      (13, 8);
    ]
  in
  Conwaycore.place_relative_cells grid ~x0 ~y0 glider_rel_coords

let place_abs_eater1 grid =
  let eater_abs_coords =
    [ (33, 33); (32, 33); (32, 32); (32, 31); (31, 30); (30, 31); (30, 30) ]
  in
  Conwaycore.place_absolute_cells grid eater_abs_coords

let place_abs_eater2 grid =
  let eater_abs_coords =
    (* [ (30, 4); (30, 5); (31, 4); (31, 6); (32, 6); (33, 6); (33, 7)  *)
    [ (4, 30); (5, 30); (4, 31); (6, 31); (6, 32); (6, 33); (7, 33) ]
  in
  Conwaycore.place_absolute_cells grid eater_abs_coords

let initial_state width height =
  let grid = initgrid ~width ~height () in
  (* let grid = place_rel_glider grid ~x0:30 ~y0:30 in *)
  (* let grid = place_abs_glider grid in *)
  let grid = place_rel_gliGun grid ~x0:0 ~y0:0 in
  let grid = place_abs_eater2 grid in
  { grid; running = false; width; height; debug_msg = ""; input_buffer = "" }

(* Generate an infinite sequence of grids *)
let rec generate_grids grid () =
  Stdlib.Seq.Cons (grid, generate_grids (update_grid grid))

(* Run the specified number of steps, displaying each step *)
let rec run_steps ~term ~state seq n =
  if n >= 0 then (
    match seq () with
    | Stdlib.Seq.Nil -> state.grid
    | Stdlib.Seq.Cons (grid, next_seq) ->
        Term.image term (nottyPPGrid ~debug_msg:state.debug_msg grid);
        Unix.sleepf 0.1;
        let state = { state with grid } in
        run_steps ~term ~state next_seq (n - 1))
  else
    state.grid

(* Main loop to handle events and update the grid *)
let rec main_loop term state =
  Term.image term (nottyPPGrid ~debug_msg:state.debug_msg state.grid);
  let state =
    if state.running then
      { state with grid = update_grid state.grid }
    else
      state
  in
  handle_events term state

(* Handle user input events *)
and handle_events term state =
  match Term.event term with
  | `Key (`ASCII 'q', _) -> Term.release term
  | `Key (`ASCII 'p', _) ->
      main_loop term { state with running = not state.running }
  | `Key (`ASCII 's', _) ->
      main_loop term
        { state with grid = update_grid state.grid; running = false }
  | `Key (`ASCII c, _) when Char.is_digit c ->
      let input_buffer = state.input_buffer ^ Char.escaped c in
      let debug_msg = Printf.sprintf "Input: %s" input_buffer in
      main_loop term { state with input_buffer; debug_msg }
  | `Key (`Enter, _) -> (
      try
        let steps = Int.of_string state.input_buffer in
        let grids = generate_grids state.grid in
        let new_grid = run_steps ~term ~state grids steps in
        let debug_msg = Printf.sprintf "Ran %d steps" steps in
        main_loop term
          { state with grid = new_grid; input_buffer = ""; debug_msg }
      with Failure _ ->
        let debug_msg = "Invalid number" in
        main_loop term { state with input_buffer = ""; debug_msg })
  | `Key (`ASCII 'e', _) ->
      let alive_cells = Conwaycore.extract_alive_cells state.grid in
      Stdlib.Printf.printf "Alive cells: [%s]\n"
        (String.concat ~sep:"; "
           (List.map alive_cells ~f:(fun (x, y) ->
                Printf.sprintf "(%d, %d)" x y)));
      Term.release term
  | `Mouse (`Press (`Left | `Right), (y, x), _) ->
      let grid =
        Agrid.mapi
          (fun ~x:grid_x ~y:grid_y cell ->
            if grid_x = x && grid_y = y / NtyConway.charWidth then
              match cell with Alive -> Dead | Dead -> Alive
            else
              cell)
          state.grid
      in
      let debug_msg = Printf.sprintf "Mouse click at (%d, %d)" x y in
      main_loop term { state with grid; debug_msg }
  | _ -> main_loop term state

(* Entry point of the program *)
let main () =
  let term = Term.create () in
  let state = initial_state 40 40 in
  main_loop term state

(* Run the main function *)
(* let () = main () *)
