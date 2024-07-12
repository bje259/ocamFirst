open Agrid

(* open Flex_array *)
open! Base
open! Ocamlutils.DebugUtil.Printing
open! Ocamlutils.DebugUtil.ConvUtils
open Notty
open Notty_unix
open PpConway
open NottyConway

(* open Types *)
open! Ocamlutils.Types
module Types = Ocamlutils.Types

(* Grid settup *)

let agFoldi f acc (grid : 'a t) =
  Flex_array.foldi
    (fun acc y row -> Flex_array.foldi (fun acc x v -> f ~x ~y acc v) acc row)
    acc grid

let () = Random.self_init ()

let randStartState () =
  if Random.bool () then
    Alive
  else
    Dead

(* Initialize a grid with a random state *)

let initgrid ?(width = 20) ?(height = 20) () =
  let emptyG =
    Agrid.of_array (Base.Array.make_matrix ~dimx:width ~dimy:height Dead)
  in
  Agrid.map (fun _ -> randStartState ()) emptyG

(* Safely get the cell, returning Dead if out of bounds *)
let get_cell (grid : Types.conwayCell Agrid.t) x y =
  if x < 0 || y < 0 || x >= Agrid.width grid || y >= Agrid.height grid then
    Dead
  else
    Agrid.get grid ~x ~y

let count_alive_neighbors2 grid x y =
  let neighbors =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  List.fold neighbors ~init:0 ~f:(fun acc (dx, dy) ->
      match get_cell grid (x + dx) (y + dy) with
      | Alive -> acc + 1
      | Dead -> acc)

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
let next_state2 grid x y =
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

(* Apply the rules to a cell *)
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

(* Update the grid *)
let update_grid grid = Agrid.mapi (fun ~x ~y _ -> next_state grid x y) grid

(* Main loop *)
(* let rec main_loop grid = *)
(*   print_grid grid; *)
(*   let next_grid = update_grid grid in *)
(*   Unix.sleepf 0.5; *)
(*   main_loop next_grid *)

(* Assume update_grid is a function that takes a grid and returns the next grid *)
let rec generate_grids grid () =
  Stdlib.Seq.Cons (grid, generate_grids (update_grid grid))

(* Function to take a fixed number of steps *)
let rec run_steps ~term seq n =
  if n > 0 then (
    match seq () with
    | Stdlib.Seq.Nil -> ()
    | Stdlib.Seq.Cons (grid, next_seq) ->
        Term.image term (nottyPPGrid2 grid);
        Unix.sleepf 0.5;
        run_steps ~term next_seq (n - 1))
  else
    ()

(* Initialize and start the game *)
let test () =
  let t = Term.create () in
  let width = 10 in
  let height = 10 in
  let initial_grid = initgrid ~width ~height () in
  (* let () = print_grid initial_grid in *)
  Term.image t (nottyPPGrid2 initial_grid);
  let grids = generate_grids initial_grid in
  let () = run_steps ~term:t grids 10 in
  let _ = Term.event t in
  Term.release t
