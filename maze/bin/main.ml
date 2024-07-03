open Lib.Types
open Lib.FunctionsNew

let mazeList (algoLst : solution_algo list) (genF : mazeGen) (n : int) (w : int)
    (h : int) : solved_maze list =
  let width, height = (w, h) in
  let mazeQueue = Queue.create () in
  let rec repeat = function
    | 0 -> ()
    | i ->
        let maze = genF width height in
        let rec solveMaze = function
          | [] -> ()
          | hd :: tl ->
              let solvedResult = Option.value ~default:[] (hd maze) in
              let solvedMaze = { smaze = maze; solution = solvedResult } in
              let () = Queue.push solvedMaze mazeQueue in
              solveMaze tl
        in
        let () = solveMaze algoLst in
        repeat (i - 1)
  in
  repeat n;
  let result = List.of_seq (Queue.to_seq mazeQueue) in
  result

let () =
  MazePrinter.print_smaze_list
    (mazeList
       [ Solver.dfs_solve_new; Solver.bfs_solve_new ]
       Generator.binary_tree_maze 1 20 10)
;;

let testA = Generator.binary_tree_maze 30 10 in
let solutionA = Solver.dfs_solve_new testA in
MazePrinter.print_mazeNew
  ~solution_path:(Option.value ~default:[] solutionA)
  testA

(*let printMazeList mazes = *)
(**)
(*    Printf.printf "Maze %d\n" i;*)
(*    print_endline "DFS Solution:";*)

(*let () = *)
(*  Random.self_init ();*)
(*  let width, height = 20, 10 in*)
(*  let maze = binary_tree_maze width height in*)
(**)
(*  (* Print the generated maze *)*)
(*  print_mazeNew maze;*)
(**)
(*  (* Solve the maze using DFS *)*)
(*  let solution_dfs_new = dfs_solve_new maze in*)
(*  print_endline "DFS Solution:";*)
(*  (*print_mazeNew ~solution_path:(Option.value ~default:[] solution_dfs_new) maze;*)*)
(**)
(*  (* Solve the maze using BFS *)*)
(*  let solution_bfs_new = bfs_solve_new maze in*)
(*  print_endline "BFS Solution:";*)
(*  (*print_mazeNew ~solution_path:(Option.value ~default:[] solution_bfs_new) maze;;*)*)

(*let () =*)
(*  Random.self_init ();*)
(*  let width, height = 20, 10 in*)
(*  let maze = binary_tree_maze width height in*)
(**)
(*  (* Print the generated maze *)*)
(*  print_mazeNew maze;*)
(**)
(*  (* Solve the maze using DFS *)*)
(*  let solution_dfsNew = Lib.Functions.dfs_solveNew maze in*)
(*  print_endline "DFS Solution:";*)
(*  print_mazeNew ~solution_path:(Option.value ~default:[] solution_dfsNew) maze;*)
(**)
(*  (* Solve the maze using BFS *)*)
(*  let solution_bfsNew = Lib.Functions.bfs_solveNew maze in*)
(*  print_endline "BFS Solution:";*)
(*  print_mazeNew ~solution_path:(Option.value ~default:[] solution_bfsNew) maze;*)
