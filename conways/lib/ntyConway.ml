open Agrid
open! Base
open! Ocamlutils.DebugUtil.Printing
open! Ocamlutils.DebugUtil.ConvUtils
open! Ocamlutils.Types
module Types = Ocamlutils.Types
open Notty
open Notty_unix
open PpConway

(* let string_of_cell = function Alive -> "■" | Dead -> " " *)
let string_of_cell cell = match cell with Alive -> "██" | Dead -> "  "
let charWidth = 2

let agFoldi f acc (grid : 'a Agrid.t) =
  Flex_array.foldi
    (fun acc y row -> Flex_array.foldi (fun acc x v -> f ~x ~y acc v) acc row)
    acc grid

let string_of_cell_neighbors grid x y =
  Int.to_string (Conwaycore.count_alive_neighbors grid x y)

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
            |> hpad (y * charWidth) 0)
        in
        let img = I.(img </> cellImg) in
        img)
      (I.void (width * charWidth) height)
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
          I.(
            string A.empty (string_of_cell cell)
            |> hpad (x * charWidth) 0
            |> vpad y 0)
        in
        let img = I.(img </> cellImg) in
        img)
      (I.void (width * charWidth) height)
      grid
  in
  let debug_img = I.string A.empty debug_msg in
  I.(img <-> debug_img)

let compare_grids old_grid new_grid =
  let width = Agrid.width old_grid in
  let height = Agrid.height old_grid in
  let img =
    agFoldi
      (fun ~x ~y img old_cell ->
        let new_cell = Conwaycore.get_cell new_grid x y in
        let color, new_cell_adjusted =
          match (old_cell, new_cell) with
          | Alive, Dead -> (A.fg A.red, string_of_cell old_cell)
          | Dead, Alive -> (A.fg A.green, string_of_cell new_cell)
          | _ -> (A.empty, string_of_cell new_cell)
        in
        let cellImg =
          I.(
            string color new_cell_adjusted |> hpad (x * charWidth) 0 |> vpad y 0)
        in
        let img = I.(img </> cellImg) in
        img)
      (I.void (width * charWidth) height)
      old_grid
  in
  img

let nottyPPGrid ~debug_msg grid =
  let width = Agrid.width grid in
  let height = Agrid.height grid in
  let img =
    agFoldi
      (fun ~x ~y img cell ->
        let cellImg =
          I.(
            string A.empty (string_of_cell cell)
            |> hpad (x * charWidth) 0
            |> vpad y 0)
        in
        let img = I.(img </> cellImg) in
        img)
      (I.void (width * charWidth) height)
      grid
  in
  let debug_img = I.string A.empty debug_msg in
  let next_grid = Conwaycore.update_grid grid in
  let diff_img = compare_grids grid next_grid in
  let dbgNeighImg = dbgGridNeighAlive ~debug_msg:"" grid in
  let dbgNextStateImg = dbgImg ~debug_msg:"" next_grid in

  I.(img <-> debug_img <|> diff_img)
