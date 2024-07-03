(*functions.ml module to house functions *)
(*open DebugUtil*)
open Types

open Queue



let string_of_cell = function
  | Wall -> "XX"
  | Path -> "  "

  let string_of_maze maze = 
    let s = ref "" in
    let height = maze.height *2 +1 in
    let width = maze.width *2 +1 in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        s := !s ^ (string_of_cell maze.grid.(y).(x))
      done;
      s := !s ^ "\n"
    done;
    !s


let init_maze width height =
  let grid = Array.make_matrix (2 * height + 1) (2 * width + 1) Wall in
  { width; height; grid };;



let find_end maze = 
  let curX = 1 in
  let curY = Array.length maze.grid - 2 in
  let rec aux maze y x = 
    match maze.grid.(y).(x) with
    |Path -> (y+1,x)
    |Wall -> aux maze y (x+1) in
aux maze curY curX;;


let binary_tree_maze width height =
  let maze = init_maze width height in
  (*Printf.printf "width: %d\nheight: %d\n" width height;*)
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let cell_y = 2 * y + 1 in
      let cell_x = 2 * x + 1 in
      (*Printf.printf "build room: (%d,%d)\n" cell_x cell_y;*)
      maze.grid.(cell_y).(cell_x) <- Path;
      let carve_north = y > 0 && (x = width - 1 || Random.bool ()) in
      if carve_north then begin
        (*Printf.printf "carve_north: (%d,%d)\n" cell_x cell_y;*)
        maze.grid.(cell_y - 1).(cell_x) <- Path;
      end
      else if x < width - 1 then begin
        (*Printf.printf "carve_east: (%d,%d)\n" cell_x cell_y;*)
        maze.grid.(cell_y).(cell_x + 1) <- Path;
      end
    done;
  done;
  let () = 
    let mzEnd = find_end maze in
    let eY = fst mzEnd in
    let eX = snd mzEnd in
    maze.grid.(eY).(eX) <- Path; 
    maze.grid.(0).(1) <- Path;
  in
  maze;;

(* Sidewinder Maze Generation *)
(*let sidewinder_maze2 width height =*)
(*  let mazesw = init_maze width height in*)
(*  for y = 0 to height - 1 do*)
(*    let run_start = ref 0 in*)
(*    for x = 0 to width - 1 do*)
(*      let cell_y = 2 * y + 1 in*)
(*      let cell_x = 2 * x + 1 in*)
(*      mazesw.grid.(cell_y).(cell_x) <- Path; *)
(*      let carve_east = x < width - 1 && (y = 0 || Random.bool ()) in*)
(*      if carve_east then mazesw.grid.(cell_y).(cell_x + 1) <- Path  *)
(*      else begin *)
(*        let run_end = !run_start + Random.int (x - !run_start + 1) in*)
(*        mazesw.grid.(cell_y - 1).(2 * run_end + 1) <- Path; *)
(*run_start := (x + 1);*)
(*      end;*)
(*    done;*)
(*  done;*)
(*    let mzEnd = find_end mazesw in*)
(*    let eY = fst mzEnd in*)
(*    let eX = snd mzEnd in*)
(*    mazesw.grid.(eY).(eX) <- Path; in *)
(*mazesw.grid.(0).(1) <- Path; in sidwinder_maze2;;*)

(* Check if a position is within the maze bounds *)
let in_bounds maze (y, x) =
  x >= 0 && y >= 0 && y < Array.length maze.grid && x < Array.length maze.grid.(0)

(* Check if a position is a path *)
let is_path maze (y, x) =
  in_bounds maze (y, x) && maze.grid.(y).(x) = Path


let neighbors (y, x) maze =
  let moves = [(-1, 0); (1, 0); (0, -1); (0, 1)] in
    let filt = List.filter (fun (dy, dx) ->
    let ny, nx = y + dy, x + dx in
    nx >= 0 && ny >= 0 && nx < (Array.length maze.grid.(0)) && ny < (Array.length maze.grid) && maze.grid.(ny).(nx) = Path
  ) moves; in
    let pot_nxts = List.map (fun (dy, dx) -> (y + dy, x + dx)) filt in

  (*let () = (DebugUtil.debug_print_list (DebugUtil.string_of_tuple (string_of_int) (string_of_int)) pot_nxts) in*)
  pot_nxts;;

let dfs_solve maze =
  let start = (0, 1) in
  let goal = find_end maze in
  let rec aux visited stack =
    match stack with
    | [] -> None
    | (y, x) :: rest ->
      if (y, x) = goal then Some ((y, x) :: visited)
      else begin
        (*Printf.printf "Visiting (%d, %d)\n" y x;*)
        (*Printf.printf "Visited: %d\n" (List.length visited);*)
        (*Printf.printf "Stack: %d\n" (List.length stack);*)
        
        let new_moves = List.filter (fun move -> not (List.mem move visited)) (neighbors (y, x) maze) in
        aux ((y, x) :: visited) (new_moves @ rest)
      end
  in
  let result = aux [] [start] in
let () = 
    match result with
    | None -> Printf.printf "No solution found\n"
    | Some path -> Printf.printf "Solution found: %d steps\n" (List.length path);
  in result;;




 (* Utility functions to move in a direction *)
let move (y, x) = function
  | North -> (y - 1, x);
  | South -> (y + 1, x);
  | East  -> (y, x + 1);
  | West  -> (y, x - 1);;



(* DFS solver *)
let solve_maze_dfs maze =
  let start = (0, 1) in
  let finish = find_end maze in
  let rec dfs path visited (y, x) =
    if (y, x) = finish then Some (List.rev ((y, x) :: path))
    else if not (is_path maze (y, x)) || List.mem (y, x) visited then None
    else
      let new_path = (y, x) :: path in
      let new_visited = (y, x) :: visited in
      List.fold_left (fun acc dir ->
        match acc with
        | Some _ -> acc
        | None -> dfs new_path new_visited (move (y, x) dir)
      ) None [North; South; East; West]
  in
  let result = dfs [] [] start in
  let () = 
    match result with
    | None -> Printf.printf "No solution found\n"
    | Some path -> Printf.printf "Solution found: %d steps\n" (List.length path);
  in result;;



(* BFS solver *)
let solve_maze_bfs maze =
  let start = (0, 1) in
  let finish = find_end maze in
  let rec bfs queue visited =
    match queue with
    | [] -> None
    | (bfs_path, (y, x)) :: rest ->
      if (y, x) = finish then Some (List.rev ((y, x) :: bfs_path))
      else if not (is_path maze (y, x)) || List.mem (y, x) visited then bfs rest visited
      else
        let new_bfs_path = (y, x) :: bfs_path in
        let new_visited = (y, x) :: visited in
        let bfs_neighbors = [move (y, x) North; move (y, x) South; move (y, x) East; move (y, x) West] in
        let new_queue = List.fold_left (fun acc neighbor -> (new_bfs_path, neighbor) :: acc) rest bfs_neighbors in
        bfs new_queue new_visited
    in
   let result_bfs = bfs [([], start)] []
    in
    let () = 
      match result_bfs with
      | None -> Printf.printf "No solution found\n"
      | Some bfs_path -> Printf.printf "Solution found: %d steps\n" (List.length bfs_path);
  in result_bfs;;



(* BFS solver *)
let solve_maze_bfs2 maze =
  let start = (0, 1) in
  let finish = find_end maze in
  let queue = create () in
  let visited = Hashtbl.create (Array.length maze.grid * Array.length maze.grid.(0)) in
  let rec bfs () =
    if is_empty queue then None
    else
      let (bfs_path, (y, x)) = take queue in
      if (y, x) = finish then Some (List.rev ((y, x) :: bfs_path))
      else if not (is_path maze (y, x)) || Hashtbl.mem visited (y, x) then bfs ()
      else begin
        Hashtbl.add visited (y, x) true;
        List.iter (fun dir ->
          let next_pos = move (y, x) dir in
          if is_path maze next_pos && not (Hashtbl.mem visited next_pos) then
            add ((y, x) :: bfs_path, next_pos) queue
        ) [North; South; East; West];
        bfs ()
      end
  in
  let bfs2_result = (add ([], start) queue;
  bfs ()) in
    let () = 
      match bfs2_result  with
      | None -> Printf.printf "No solution found\n"
      | Some bfs_path -> Printf.printf "Solution found: %d steps\n" (List.length bfs_path);
  in bfs2_result;;




let print_maze2 ?solve maze =
  let wll = "██" in
  let pth = "  " in
  let solved = "••" in

  let solution_path = 
    match solve with
    |None -> [];
    |Some Solution_path p -> p;
    |Some Solution_algo solve -> Option.value ~default:[] (solve maze); in

  Array.iteri (fun y row ->
    Array.iteri (fun x cell ->
      match cell with
      | Wall -> print_string wll
      | Path ->
        if List.mem (y, x) solution_path then
          print_string solved
        else
          print_string pth
    ) row;
    print_newline ()
  ) maze.grid


let dfs_solve_new maze =
  let start = (0, 1) in
  let goal = find_end maze in
  let dfsfailed_attempts = ref 0 in
  let dfsattempts = ref 0 in
  let rec aux visited stack =
    dfsattempts := !dfsattempts + 1;
    match stack with
    | [] -> None
    | (y, x) :: rest ->
      if (y, x) = goal then Some (List.rev ((y, x) :: visited))
      else if List.mem (y, x) visited then
        let () = incr dfsfailed_attempts in
        aux visited rest
      else
        let new_moves = List.filter (fun move -> not (List.mem move visited)) (neighbors (y, x) maze) in
        if new_moves = [] then
          let () = incr dfsfailed_attempts in
          aux visited rest
        else
          aux ((y, x) :: visited) (new_moves @ rest)
  in
  let dfsresult = aux [] [start] in
  Printf.printf "DFS failed attempts: %d\n" !dfsfailed_attempts;
  Printf.printf "DFS attempts: %d\n" !dfsattempts;
  dfsresult

let bfs_solve_new maze =
  let start = (0, 1) in
  let goal = find_end maze in
  let queue = Queue.create () in
  let visited = Hashtbl.create (Array.length maze.grid * Array.length maze.grid.(0)) in
  let bfsfailed_attempts = ref 0 in
  let bfsattempts = ref 0 in
  let rec bfs () =
    bfsattempts := !bfsattempts + 1;
    if Queue.is_empty queue then None
    else
      let (path, (y, x)) = Queue.take queue in
      if (y, x) = goal then Some (List.rev ((y, x) :: path))
      else if Hashtbl.mem visited (y, x) then
        let () = incr bfsfailed_attempts in
        bfs ()
      else begin
        Hashtbl.add visited (y, x) true;
        (*let new_moves = List.filter (fun (dy, dx) ->*)
        (*  let next_pos = (y + dy, x + dx) in*)
        (*  is_path maze next_pos && not (Hashtbl.mem visited next_pos)*)
        (* ) [(-1, 0); (1, 0); (0, -1); (0, 1)] in*)
        List.iter (fun (dy, dx) ->
          let next_pos = (y + dy, x + dx) in
          if is_path maze next_pos && not (Hashtbl.mem visited next_pos) then
            Queue.add ((y, x) :: path, next_pos) queue
          else
            incr bfsfailed_attempts
        ) [(-1, 0); (1, 0); (0, -1); (0, 1)];
        bfs ()
      end
  in
  Queue.add ([], start) queue;
  let bfsresult = bfs () in
  Printf.printf "BFS failed attempts: %d\n" !bfsfailed_attempts;
  Printf.printf "BFS attempts: %d\n" !bfsattempts;
  bfsresult



let dfs_solveNew maze =
  let start = (0, 1) in
  let goal = find_end maze in
  let dfsfailed_attempts = ref 0 in
  let dfsattempts = ref 0 in
  let rec aux visited stack =
    dfsattempts := !dfsattempts + 1;
    match stack with
    | [] -> None
    | (y, x) :: rest ->
      if (y, x) = goal then Some (List.rev ((y, x) :: visited))
      else if List.mem (y, x) visited then
        let () = incr dfsfailed_attempts in
        aux visited rest
      else
        let new_moves = List.filter (fun move -> not (List.mem move visited)) (neighbors (y, x) maze) in
        aux ((y, x) :: visited) (new_moves @ rest)
  in
  let dfsresult = aux [] [start] in
  Printf.printf "DFS failed attempts: %d\n" !dfsfailed_attempts;
  Printf.printf "DFS attempts: %d\n" !dfsattempts;
  dfsresult;;

let bfs_solveNew maze =
  let start = (0, 1) in
  let goal = find_end maze in
  let queue = Queue.create () in
  let visited = Hashtbl.create (Array.length maze.grid * Array.length maze.grid.(0)) in
  let bfsfailed_attempts = ref 0 in
  let bfsattempts = ref 0 in
  let rec bfs () =
    bfsattempts := !bfsattempts + 1;
    if Queue.is_empty queue then None
    else
      let (path, (y, x)) = Queue.take queue in
      if (y, x) = goal then Some (List.rev ((y, x) :: path))
      else if Hashtbl.mem visited (y, x) then
        let () = incr bfsfailed_attempts in
        bfs ()
      else begin
        Hashtbl.add visited (y, x) true;
        List.iter (fun (dy, dx) ->
          let next_pos = (y + dy, x + dx) in
          if is_path maze next_pos && not (Hashtbl.mem visited next_pos) then
            Queue.add ((y, x) :: path, next_pos) queue
          else
            incr bfsfailed_attempts
        ) [(-1, 0); (1, 0); (0, -1); (0, 1)];
        bfs ()
      end
  in
  Queue.add ([], start) queue;
  let bfsresult = bfs () in
  Printf.printf "BFS failed attempts: %d\n" !bfsfailed_attempts;
  Printf.printf "BFS attempts: %d\n" !bfsattempts;
  bfsresult;;


let print_mazeNew ?(solution_path=[]) maze =
  let wll = "██" in
  let pth = "  " in
  let solved = "••" in
  Array.iteri (fun y row ->
    Array.iteri (fun x cell ->
      match cell with
      | Wall -> print_string wll
      | Path ->
        if List.mem (y, x) solution_path then
          print_string solved
        else
          print_string pth
    ) row;
    print_newline ()
  ) maze.grid


let sidewinder_maze width height =
  let maze = init_maze width height in
  for y = 0 to height - 1 do
    let run = ref [] in
    for x = 0 to width - 1 do
      maze.grid.(y).(x) <- Path;
      run := (x, y) :: !run;
      let should_close_out = (x = width - 1) || (y > 0 && Random.bool ()) in
      if should_close_out then
        let (rx, ry) = List.nth !run (Random.int (List.length !run)) in
        if y > 0 then maze.grid.(ry - 1).(rx) <- Path;
        run := []
      else
        if x < width - 1 then maze.grid.(y).(x + 1) <- Path;
    done;
  done;
  maze



(*let print_maze ?solve maze = *)
(*let wll = "██" in*)
(*let pth = "  " in*)
(*let solved = "••" in*)
(*let solution = *)
(*  match solve with*)
(*  | None -> None*)
(*  | Some _ -> Some (Option.value ~default:[] (solve_maze_dfs maze)) *)
(*in*)
(*  ( *)
(*  (*horizontalBorder maze;*)*)
(*  Array.iteri (fun y row ->*)
(*    (*print_char ' '; (* vertical border *)*)*)
(*    (*print_char '#'; (* vertical border *)*)*)
(*    Array.iteri (fun x cell ->*)
(*      match cell with*)
(*      | Wall -> *)
(*          print_string wll;*)
(*          (*print_char '      *)*)
(*          (*print_endline "!!";*)*)
(*      | Path ->*)
(*          match solution with*)
(*          | None -> print_string pth;*)
(*          | Some [] -> print_string pth;*)
(*          | Some spath -> *)
(*            if (List.mem (y,x) spath) then*)
(*            print_string solved*)
(*            else*)
(*            print_string pth;*)
(*          (*print_char 'A'*)*)
(*          (*print_endline "||";*)*)
(*    ) row;*)
(*    (*print_char ' '  ;  (* vertical border *)*)*)
(*    (*print_char '#'  ;  (* vertical border *)*)*)
(*    print_newline ())*)
(* maze.grid ; *)
(*  )                           *)
(*;;*)
(**)
(**)
(**)





(*let horizontalBorder maze = print_endline (String.make ( maze.width + 2) '#');;*)


(*let print_maze ?solve maze = *)
(*let wll = "██" in*)
(*let pth = "  " in*)
(*let solved = "••" in*)
(*let solution = *)
(*  match solve with*)
(*  | None -> None*)
(*  | Some _ -> Some (Option.value ~default:[] (solve_maze_dfs maze)) *)
(*in*)
(*  ( *)
(*  Array.iteri (fun y row ->*)
(*    Array.iteri (fun x cell ->*)
(*      match cell with*)
(*      | Wall -> *)
(*          print_string wll;*)
(*      | Path ->*)
(*          match solution with*)
(*          | None -> print_string pth;*)
(*          | Some [] -> print_string pth;*)
(*          | Some spath -> *)
(*            if (List.mem (y,x) spath) then*)
(*            print_string solved*)
(*            else*)
(*            print_string pth;*)
(*    ) row;*)
(*    print_newline ())*)
(* maze.grid ; *)
(*  )                           *)
(*;;*)
