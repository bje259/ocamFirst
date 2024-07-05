(*
calcDirOpen=lambda(mazeRenderTbl,CellTypeTable,let(
    walls,CellTypeTable=2,
    tiles,CellTypeTable=1,
    openWalls, walls*mazeRenderTbl,
    leftWallOpen,hstack(clib.MakeStaticArray(61,1,0),drop(openWalls,,-1)),
    northWallOpen,vstack(clib.MakeStaticArray(1,121,0),drop(openWalls,-1)),
    rightWallOpen,hstack(drop(openWalls,,1),clib.MakeStaticArray(61,1,0)),
    southWallOpen,vstack(drop(openWalls,-1),clib.MakeStaticArray(1,121,0)),
    leftBit,leftWallOpen*2,
    rightBit,rightWallOpen*8,
    northBit,northWallOpen*4,
    southBit,southWallOpen*1,
    combined,leftBit+rightBit+northBit+southBit,
    onlyTiles,combined*tiles,
    red,reduce(lib.empty_set,combined,lambda(a,c,lib.union(a,c))),
    //using bitwise flagging for directions, 8=East,4=North,2=West,1=South
    //identify tiles where east isn't the only option
    eastPlus,map(onlyTiles,lambda(_,AND((BITAND(_,8)=8),_>8)+0)),
    //identify where west is best option
    westOpt,map(onlyTiles,lambda(_,(AND(BITAND(_,2)=2,_<=2))+0)),
    westOptShift,HSTACK(DROP(westOpt,,2),clib.MakeStaticArray(61,2,0)),
    overlay,eastPlus*westOptShift*-8+onlyTiles,
    letters,LAMBDA(_,IFS(BITAND(_,8)=8,"E",BITAND(_,4)=4,"N",BITAND(_,2)=2,"W",BITAND(_,1)=1,"S",TRUE,"")),
    result, map(overlay,letters),
    onlyTiles
));
*)
open Types

(* open FunctionsNew *)
open Base

let checkCell (x : int) (y : int) (c : cell) =
  match c with
  | Wall -> 2
  | Path when (not (x % 2 = 0)) && not (y % 2 = 0) -> 1
  | Path -> 0

let testmaze = FunctionsNew.Generator.binary_tree_maze 20 10
let grid = Agrid.of_array testmaze.grid
let newgrid = Agrid.mapi (fun ~x ~y i -> checkCell x y i) grid
(* let buildEmptyGrid (x : int) (y : int) = Flex_array.make x (Flex_array.make y 0) *)
(* let wallsGrid = buildEmptyGrid (Agrid.width newgrid) (Agrid.height newgrid) *)
(* let tilesGrid = buildEmptyGrid (Agrid.width newgrid) (Agrid.height newgrid) *)
(* let openWallsGrid = buildEmptyGrid (Agrid.width newgrid) (Agrid.height newgrid) *)

(* let wallsGrid, tilesGrid, openWallsGrid = *)
(*   Agrid.mapi *)
(*     (fun ~x:xi ~y:yi i -> *)
(*       match i with *)
(*       | 2 -> (Agrid.set wallsGrid ~x:xi ~y:yi 1, tilesGrid, openWallsGrid) *)
(*       | 1 -> (wallsGrid, Agrid.set tilesGrid ~x:xi ~y:yi 1, openWallsGrid) *)
(*       | 0 -> (wallsGrid, tilesGrid, Agrid.set openWallsGrid ~x:xi ~y:yi 1)) *)
(*     newgrid *)

let wallsGrid =
  Agrid.map
    (fun i ->
      if i = 2 then
        1
      else
        0)
    newgrid

let tilesGrid =
  Agrid.map
    (fun i ->
      if i = 1 then
        1
      else
        0)
    newgrid

let openWallsGrid =
  Agrid.map
    (fun i ->
      if i = 0 then
        1
      else
        0)
    newgrid

let esMain () =
  Stdlib.print_endline "Hello, World!";
  FunctionsNew.MazePrinter.print_mazeNew testmaze;
  Stdlib.print_endline "Cell Type";
  DebugUtil.ppGrid newgrid Stdlib.Format.pp_print_int;
  Stdlib.print_endline "Walls";
  DebugUtil.ppGrid wallsGrid Stdlib.Format.pp_print_int;
  Stdlib.print_endline "Tiles";
  DebugUtil.ppGrid tilesGrid Stdlib.Format.pp_print_int;
  Stdlib.print_endline "Open Walls";
  DebugUtil.ppGrid openWallsGrid Stdlib.Format.pp_print_int
