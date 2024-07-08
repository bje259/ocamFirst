type cell =
  | Wall
  | Path

type maze =
  { width : int
  ; height : int
  ; grid : cell array array
  }

type mazeGen = int -> int -> maze

(* Define the type for directions *)
type direction =
  | North
  | South
  | East
  | West

type solution_path = (int * int) list
type solution_algo = maze -> (int * int) list option

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

type 'a transformer =
  | Identity
  | Function of ('a -> string)
