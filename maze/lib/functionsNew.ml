open Types

(*open Queue*)
module Maze = struct
  let init_maze width height =
    let grid = Array.make_matrix ((2 * height) + 1) ((2 * width) + 1) Wall in
    { width; height; grid }
end

module Solver = struct
  (*open Maze*)

  let find_end maze =
    let rec aux y x =
      match maze.grid.(y).(x) with Path -> (y + 1, x) | Wall -> aux y (x + 1)
    in
    aux (Array.length maze.grid - 2) 1

  let find_start maze =
    let rec aux y x =
      match maze.grid.(y).(x) with Path -> (y, x) | Wall -> aux y (x + 1)
    in
    aux 0 1

  (* Check if a position is within the maze bounds *)
  let in_bounds maze (y, x) =
    x >= 0 && y >= 0
    && y < Array.length maze.grid
    && x < Array.length maze.grid.(0)

  (* Check if a position is a path *)
  let is_path maze (y, x) = in_bounds maze (y, x) && maze.grid.(y).(x) = Path

  let neighbors (y, x) maze =
    let moves = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] in
    let filt =
      List.filter
        (fun (dy, dx) ->
          let ny, nx = (y + dy, x + dx) in
          nx >= 0 && ny >= 0
          && nx < Array.length maze.grid.(0)
          && ny < Array.length maze.grid
          && maze.grid.(ny).(nx) = Path)
        moves
    in
    let pot_nxts = List.map (fun (dy, dx) -> (y + dy, x + dx)) filt in
    pot_nxts

  let dfs_solve_new maze =
    (* let start = (0, 1) in *)
    let start = find_start maze in
    let goal = find_end maze in
    let dfsfailed_attempts = ref 0 in
    let dfsattempts = ref 0 in
    let rec aux visited stack =
      dfsattempts := !dfsattempts + 1;
      match stack with
      | [] -> None
      | (y, x) :: rest ->
          if (y, x) = goal then
            Some (List.rev ((y, x) :: visited))
          else if List.mem (y, x) visited then
            let () = incr dfsfailed_attempts in
            aux visited rest
          else
            let new_moves =
              List.filter
                (fun move -> not (List.mem move visited))
                (neighbors (y, x) maze)
            in
            if new_moves = [] then
              let () = incr dfsfailed_attempts in
              aux visited rest
            else
              aux ((y, x) :: visited) (new_moves @ rest)
    in
    let result = aux [] [ start ] in
    let () =
      Printf.printf "DFS attempts: %d\n" !dfsattempts;
      Printf.printf "DFS failed attempts: %d\n" !dfsfailed_attempts
    in
    result

  let bfs_solve_new maze =
    (* let start = (0, 1) in *)
    let start = find_start maze in
    let goal = find_end maze in
    let queue = Queue.create () in
    let visited =
      Hashtbl.create (Array.length maze.grid * Array.length maze.grid.(0))
    in
    let bfsfailed_attempts = ref 0 in
    (* let bfsattempts = ref 0 in *)
    let rec bfs () =
      if Queue.is_empty queue then
        None
      else
        let path, (y, x) = Queue.take queue in
        if (y, x) = goal then
          Some (List.rev ((y, x) :: path))
        else if Hashtbl.mem visited (y, x) then
          let () = incr bfsfailed_attempts in
          bfs ()
        else (
          Hashtbl.add visited (y, x) true;
          List.iter
            (fun (dy, dx) ->
              let next_pos = (y + dy, x + dx) in
              if is_path maze next_pos && not (Hashtbl.mem visited next_pos)
              then
                Queue.add ((y, x) :: path, next_pos) queue
              else
                incr bfsfailed_attempts)
            [ (-1, 0); (1, 0); (0, -1); (0, 1) ];
          bfs ())
    in
    let () = Queue.add ([], start) queue in
    let result = bfs () in
    let hashList = List.of_seq (Hashtbl.to_seq_keys visited) in
    let bfsattempts = List.length hashList in
    (* let () = *)
    (*   DebugUtil.debug_print_list *)
    (*     (DebugUtil.string_of_tuple string_of_int string_of_int) *)
    (*     hashList *)
    (* in *)
    let () = Printf.printf "BFS attempts: %d\n" bfsattempts in
    let () = Printf.printf "BFS failed attempts: %d\n" !bfsfailed_attempts in
    result
end

module Generator = struct
  open Maze

  let () = Random.self_init ()

  let binary_tree_maze width height =
    let maze = init_maze width height in
    (*Printf.printf "width: %d\nheight: %d\n" width height;*)
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let cell_y = (2 * y) + 1 in
        let cell_x = (2 * x) + 1 in
        (*Printf.printf "build room: (%d,%d)\n" cell_x cell_y;*)
        maze.grid.(cell_y).(cell_x) <- Path;
        let carve_north = y > 0 && (x = width - 1 || Random.int 10 < 4) in
        if carve_north then
          (*Printf.printf "carve_north: (%d,%d)\n" cell_x cell_y;*)
          maze.grid.(cell_y - 1).(cell_x) <- Path
        else if x < width - 1 then
          (*Printf.printf "carve_east: (%d,%d)\n" cell_x cell_y;*)
          maze.grid.(cell_y).(cell_x + 1) <- Path
      done
    done;
    let () =
      let mzEnd = Solver.find_end maze in
      let eY = fst mzEnd in
      let eX = snd mzEnd in
      maze.grid.(eY).(eX) <- Path;
      maze.grid.(0).(1) <- Path
    in
    maze

  (*Sidewinder Maze Generation *)
  let sidewinder_maze2 width height =
    let mazesw = init_maze width height in
    for y = 0 to height - 1 do
      let run_start = ref 0 in
      for x = 0 to width - 1 do
        let cell_y = (2 * y) + 1 in
        let cell_x = (2 * x) + 1 in
        mazesw.grid.(cell_y).(cell_x) <- Path;
        let carve_east = x < width - 1 && (y = 0 || Random.int 10 < 7) in
        if carve_east then
          mazesw.grid.(cell_y).(cell_x + 1) <- Path
        else
          let run_end = !run_start + Random.int (x - !run_start + 1) in
          mazesw.grid.(cell_y - 1).((2 * run_end) + 1) <- Path;
          run_start := x + 1
      done
    done;
    let () =
      let mzEnd = Solver.find_end mazesw in
      let eY = fst mzEnd in
      let eX = snd mzEnd in
      mazesw.grid.(eY).(eX) <- Path
    in

    (* Remove the hard-coded start point *)
    (* mazesw.grid.(0).(1) <- Path; *)
    let result = mazesw in
    result
end

module MazePrinter = struct
  (*open Maze*)

  let print_mazeNew ?(solution_path = []) maze =
    let wll = "██" in
    let pth = "  " in
    let solved = "••" in
    Array.iteri
      (fun y row ->
        Array.iteri
          (fun x cell ->
            match cell with
            | Wall -> print_string wll
            | Path ->
                if List.mem (y, x) solution_path then
                  print_string solved
                else
                  print_string pth)
          row;
        print_newline ())
      maze.grid

  let print_maze2 ?solve maze =
    let wll = "██" in
    let pth = "  " in
    let solved = "••" in

    let solution_path =
      match solve with
      | None -> []
      | Some (Solution_path p) -> p
      | Some (Solution_algo solve) -> Option.value ~default:[] (solve maze)
    in

    Array.iteri
      (fun y row ->
        Array.iteri
          (fun x cell ->
            match cell with
            | Wall -> print_string wll
            | Path ->
                if List.mem (y, x) solution_path then
                  print_string solved
                else
                  print_string pth)
          row;
        print_newline ())
      maze.grid

  let print_smaze_list smaze_list =
    List.iter
      (fun { smaze; solution } ->
        print_mazeNew ~solution_path:solution smaze;
        print_newline ())
      smaze_list

  let mazeList (algoLst : solution_algo list) (genF : mazeGen) (n : int)
      (w : int) (h : int) : solved_maze list =
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
          let () =
            Printf.printf "Maze %d\n" (n - i + 1);
            solveMaze algoLst
          in
          repeat (i - 1)
    in
    repeat n;
    let result = List.of_seq (Queue.to_seq mazeQueue) in
    result

  let string_of_cell = function Wall -> "XX" | Path -> "  "

  let string_of_maze maze =
    let s = ref "" in
    let height = (maze.height * 2) + 1 in
    let width = (maze.width * 2) + 1 in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        s := !s ^ string_of_cell maze.grid.(y).(x)
      done;
      s := !s ^ "\n"
    done;
    !s

  let pp_print_cell fmt cell =
    Stdlib.Format.fprintf fmt "%s" (string_of_cell cell)
end
