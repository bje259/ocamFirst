
type cell = Wall | Path

type maze = {
  width: int;
  height: int;
  grid: cell array array;
}

type mazeGen = (int -> int -> maze)

(* Define the type for directions *)
type direction = North | South | East | West

type solution_path = ((int * int) list)
type solution_algo = (maze -> (int * int) list option)

type solution_arg = 
  | Solution_path of solution_path 
  | Solution_algo of solution_algo

type solved_maze = {
  smaze: maze;
  solution: solution_path;
}



