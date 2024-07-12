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
open Notty
open Notty_unix
open PpConway

let agFoldi f acc (grid : 'a t) =
  Flex_array.foldi
    (fun acc y row -> Flex_array.foldi (fun acc x v -> f ~x ~y acc v) acc row)
    acc grid

let nottyPPGrid2 grid =
  let width = Agrid.width grid in
  let height = Agrid.height grid in
  let img = I.void width (height + 1) in
  let img =
    agFoldi
      (fun ~x ~y img cell ->
        let cellImg =
          I.(string A.empty (string_of_cell cell) |> hpad (x * 2) 0 |> vpad y 0)
        in
        let img = I.(img </> cellImg) in
        img)
      img grid
  in
  img

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

let string_of_cell_neighbors grid x y =
  Int.to_string (count_alive_neighbors grid x y)

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

let dbgGridNeighAlive ~debug_msg grid =
  let width = Agrid.width grid in
  let height = Agrid.height grid in
  let img =
    agFoldi
      (fun ~x ~y img _ ->
        let cellImg =
          I.(
            string A.empty (string_of_cell_neighbors grid x y)
            |> vpad x 0
            |> hpad (y * 2) 0)
        in
        let img = I.(img </> cellImg) in
        img)
      (I.void (width * 2) height)
      grid
  in
  let debug_img = I.string A.empty debug_msg in
  I.(img <-> debug_img)

let dbgImg ~debug_msg grid =
  let width = Agrid.width grid in
  let height = Agrid.height grid in
  let img =
    agFoldi
      (fun ~x ~y img cell ->
        let cellImg =
          I.(string A.empty (string_of_cell cell) |> vpad x 0 |> hpad (y * 2) 0)
        in
        let img = I.(img </> cellImg) in
        img)
      (I.void (width * 2) height)
      grid
  in
  let debug_img = I.string A.empty debug_msg in
  I.(img <-> debug_img)

(* Function to generate the Notty image of the grid with an extra row for debug messages *)
let nottyPPGrid ~debug_msg grid =
  let width = Agrid.width grid in
  let height = Agrid.height grid in
  let img =
    agFoldi
      (fun ~x ~y img cell ->
        let cellImg =
          I.(string A.empty (string_of_cell cell) |> vpad x 0 |> hpad (y * 2) 0)
        in
        let img = I.(img </> cellImg) in
        img)
      (I.void (width * 2) height)
      grid
  in
  let debug_img = I.string A.empty debug_msg in
  let dbgNeighImg = dbgGridNeighAlive ~debug_msg:"" grid in
  let dbgNextStateImg = dbgImg ~debug_msg:"" (update_grid grid) in

  I.(img <-> debug_img <|> dbgNextStateImg <|> dbgNeighImg)
