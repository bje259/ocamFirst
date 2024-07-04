module Maze : sig
  val init_maze : int -> int -> Types.maze
end

module Solver : sig
  val find_end : Types.maze -> int * int
  val in_bounds : Types.maze -> int * int -> bool
  val is_path : Types.maze -> int * int -> bool
  val neighbors : int * int -> Types.maze -> (int * int) list
  val dfs_solve_new : Types.maze -> (int * int) list option
  val bfs_solve_new : Types.maze -> (int * int) list option
end

module Generator : sig
  val binary_tree_maze : int -> int -> Types.maze
  val sidewinder_maze2 : int -> int -> Types.maze
end

module MazePrinter : sig
  val print_mazeNew : ?solution_path:(int * int) list -> Types.maze -> unit
  val print_maze2 : ?solve:Types.solution_arg -> Types.maze -> unit
  val print_smaze_list : Types.solved_maze list -> unit

  val mazeList :
    Types.solution_algo list ->
    Types.mazeGen ->
    int ->
    int ->
    int ->
    Types.solved_maze list

  val string_of_cell : Types.cell -> string
  val string_of_maze : Types.maze -> string
end
