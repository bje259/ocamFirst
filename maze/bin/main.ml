open Lib.Types
open Lib.FunctionsNew
open Lib.ExcelSolver2
open Lib.DebugUtil
open Lib.VectorArith
open Agrid
open Base

let intPP = Stdlib.Format.pp_print_int
let print = Stdlib.print_endline
let testmaze = (Generator.binary_tree_maze : int -> int -> maze) 10 5
let fun_name () = Agrid.of_array testmaze.grid
let testMGrid = fun_name ()
let mWidth = (Agrid.width : 'a Agrid.t -> int) testMGrid
let mHeight = (Agrid.height : 'a Agrid.t -> int) testMGrid
let wallsGrid = Flex_array.make mHeight (Flex_array.make mWidth 0)
let tilesGrid = Flex_array.make mHeight (Flex_array.make mWidth 0)
let openWallsGrid = Flex_array.make mHeight (Flex_array.make mWidth 0)

let wallsgrid, tilesgrid, openwallsgrid, _, _ =
  Agrid.fold
    (fun (wg, tg, owg, x, y) i ->
      match checkCell x y i with
      | 2 ->
          calc1
            { wallsGrid = wg; tilesGrid = tg; openWallsGrid = owg; x; y }
            "Wall (%d, %d)\n"
      | 1 ->
          calc2
            { wallsGrid = wg; tilesGrid = tg; openWallsGrid = owg; x; y }
            "Tile (%d, %d)\n"
      | 0 ->
          (* let () = Stdlib.Printf.printf "Open Wall (%d, %d)\n" x y in *)
          (wg, tg, Agrid.set owg ~x ~y 1, incX x, incY y x)
      | _ -> (wg, tg, owg, incX x, incY y x))
    (wallsGrid, tilesGrid, openWallsGrid, 0, 0)
    testMGrid

let () =
  Stdlib.print_newline ();
  MazePrinter.print_maze2 testmaze;
  print (MazePrinter.string_of_maze testmaze);
  print "wallsGrid";
  ppGrid wallsgrid intPP;
  print "tilesGrid";
  ppGrid tilesgrid intPP;
  print "openwallsGrid";
  ppGrid openwallsgrid intPP

let emptyCol = Flex_array.make mHeight 0
let emptyRow = Flex_array.make mWidth 0
let leftOpenWalls = Agrid.liat_col (Agrid.cons_col emptyCol openwallsgrid)
let northOpenWalls = Agrid.liat_row (Agrid.cons_row emptyRow openwallsgrid)
let rightOpenWalls = Agrid.tail_col (Agrid.snoc_col openwallsgrid emptyCol)
let southOpenWalls = Agrid.tail_row (Agrid.snoc_row openwallsgrid emptyRow)
let leftBit = Agrid.map (fun x -> x * 2) leftOpenWalls
let rightBit = Agrid.map (fun x -> x * 8) rightOpenWalls
let northBit = Agrid.map (fun x -> x * 4) northOpenWalls
let southBit = Agrid.map (fun x -> x * 1) southOpenWalls
let gridAry = Flex_array.of_list [ leftBit; rightBit; northBit; southBit ]
let combined = GridMath.sumGridArray gridAry
let gridToAryTest = gridToAry leftBit
let gridToAryTest2 = gridToAry rightBit

(* let intGridToStringArray gIn = *)
(*   let aryStd = gridToAry gIn in *)
(*   let stringArray = int_to_string_array aryStd in *)
(*   stringArray *)

let () =
  let stringArrayleftOpen =
    intGridToStringArray ~printOpt:true ~desc:"leftOpen" leftOpenWalls
  in
  let stringArrayrightOpen =
    intGridToStringArray ~printOpt:true ~desc:"rightOpen" rightOpenWalls
  in
  let stringArraynorthOpen =
    intGridToStringArray ~printOpt:true ~desc:"northOpen" northOpenWalls
  in
  let stringArraysouthOpen =
    intGridToStringArray ~printOpt:true ~desc:"southOpen" southOpenWalls
  in

  let mergedArraysLR =
    hstack_with_margin stringArrayrightOpen
      (Array.map ~f:(Array.map ~f:MazePrinter.string_of_cell) testmaze.grid)
      5
  in
  ()
;;

(* //using bitwise flagging for directions, 8=East,4=North,2=West,1=South *)
(* //identify tiles where east isn't the only option *)
(* eastPlus,map(onlyTiles,lambda(_,AND((BITAND(_,8)=8),_>8)+0)), *)
(* //identify where west is best option *)
(* westOpt,map(onlyTiles,lambda(_,(AND(BITAND(_,2)=2,_<=2))+0)), *)
(* westOptShift,HSTACK(DROP(westOpt,,2),clib.MakeStaticArray(61,2,0)), *)
(* overlay,eastPlus*westOptShift*-8+onlyTiles,  (fun x y -> if x = 1 then y * -8 else y) *)
(* letters,LAMBDA(_,IFS(BITAND(_,8)=8,"E",BITAND(_,4)=4,"N",BITAND(_,2)=2,"W",BITAND(_,1)=1,"S",TRUE,"")), *)
(* //identify where west is best option *)
(* westOpt,map(onlyTiles,lambda(_,(AND(BITAND(_,2)=2,_<=2))+0)), *)
(* westOptShift,HSTACK(DROP(westOpt,,2),clib.MakeStaticArray(61,2,0)), *)
(* westOptShift,HSTACK(DROP(westOpt,,2),clib.MakeStaticArray(61,2,0)), *)
(* overlay,eastPlus*westOptShift*-8+onlyTiles, *)
(* letters,LAMBDA(_,IFS(BITAND(_,8)=8,"E",BITAND(_,4)=4,"N",BITAND(_,2)=2,"W",BITAND(_,1)=1,"S",TRUE,"")), *)
(* result, map(overlay,letters), *)

let eastPlus =
  Agrid.map
    (fun x ->
      if x > 8 && dirTest 8 x then
        1
      else
        0)
    onlyTiles
in
let westOpt =
  Agrid.map
    (fun x ->
      if x <= 2 && dirTest 2 x then
        1
      else
        0)
    onlyTiles
in
let westOptShift =
  Agrid.snoc_col (Agrid.liat_col (Agrid.liat_col westOpt)) emptyCol
in
let overlay =
  let calc1 =
    Lib.VectorArith.GridMath.mathGrids Lib.VectorArith.FlexVector.mul eastPlus
      westOptShift
  in
  let calc2 = Agrid.map (fun ele -> ele * -8) calc1 in
  let calc3 =
    Lib.VectorArith.GridMath.mathGrids Lib.VectorArith.FlexVector.add calc2
      onlyTiles
  in
  calc3
in
westOpt

(* todo figure out which example to use *)
