
 open Types 
 open Base 
 open Queue 
 open Agrid 


 let checkCell (x : int) (y : int) (c : cell) = 
   match c with 
   | Wall -> 2 
   | Path when (not (x % 2 = 0)) && not (y % 2 = 0) -> 1 
   | Path -> 0 

 let testmaze = (FunctionsNew.Generator.binary_tree_maze : int -> int -> maze) 10 5 
 let fun_name () = Agrid.of_array  testmaze.grid 

 let grid = fun_name () 
 let mWidth = (Agrid.width : 'a Agrid.t -> int) grid 
 let mHeight = (Agrid.height : 'a Agrid.t -> int) grid 

 let buildEmptyGrid width height = 
   Flex_array.make height ((Flex_array.make : int -> 'a -> 'a Flex_array.t) width 0) 

 let wallsGrid = (Flex_array.make mHeight (Flex_array.make mWidth 0)) 
 let tilesGrid = (Flex_array.make mHeight (Flex_array.make mWidth 0)) 
 let openWallsGrid = (Flex_array.make mHeight (Flex_array.make mWidth 0)) 

 let incX x = 
   if x = mWidth - 1 then 
     0 
   else 
     x + 1 

 let incY y x = 
   if x = mWidth - 1 then 
     y + 1 
   else 
     y 


 type parms = { 
   wallsGrid : int Agrid.t; 
   tilesGrid : int Agrid.t; 
   openWallsGrid : int Agrid.t; 
     x : int;
   y : int 
 }

 let calc1 parms s = 
     let () = Stdlib.Printf.printf s parms.x parms.y in 
     (Agrid.set parms.wallsGrid ~x:parms.x ~y:parms.y 1, parms.tilesGrid, parms.openWallsGrid, incX parms.x, incY parms.y parms.x)
 let calc2 parms s = 
     let () = Stdlib.Printf.printf s parms.x parms.y in 
     (parms.wallsGrid, Agrid.set parms.tilesGrid ~x:parms.x ~y:parms.y 1, parms.openWallsGrid, incX parms.x, incY parms.y parms.x)
 
 let wallsGrid,tilesGrid,openWallsGrid , _, _ = 
   (Agrid.fold (fun (wg, tg, owg, x, y) i -> 
       match checkCell x y i with 
     | 2 ->calc1 {wallsGrid = wg; tilesGrid = tg; openWallsGrid = owg; x = x; y = y} "Wall (%d, %d)\n"  
       | 1 -> 
         calc2  {wallsGrid = wg; tilesGrid = tg; openWallsGrid = owg;x = x; y = y}  "Tile (%d, %d)\n"  

                 | 0 -> 
           Stdlib.Printf.printf "Open Wall (%d, %d)\n" x y  
           (wg, tg, owg ~x ~y 1, (if x = (mWidth - 1) then 0 else x + 1), incY y x) 
       | _ -> (wg, tg, owg, (if x = (mWidth - 1) then 0 else x + 1), incY y x)) 
     (wallsGrid, tilesGrid, openWallsGrid, 0, 0) 
     (grid : int) 

 let emptyCol = Flex_array.make mHeight 0 
 let emptyRow = Flex_array.make mWidth 0 
 let leftOpenWalls = Agrid.liat_col (Agrid.cons_col emptyCol openWallsGrid) 
 let northOpenWalls = Agrid.liat_row (Agrid.cons_row emptyRow openWallsGrid) 
 let rightOpenWalls = Agrid.tail_col (Agrid.snoc_col openWallsGrid emptyCol) 
 let southOpenWalls = Agrid.tail_row (Agrid.snoc_row openWallsGrid emptyRow) 
 let leftBit = Agrid.map (fun x -> x * 2) leftOpenWalls 
 let rightBit = Agrid.map (fun x -> x * 8) rightOpenWalls 
 let northBit = Agrid.map (fun x -> x * 4) northOpenWalls 
 let southBit = Agrid.map (fun x -> x * 1) southOpenWalls 
 let gridAry = Flex_array.of_list [ leftBit; rightBit; northBit; southBit ] 
 let combined = VectorArith.GridMath.sumGridArray gridAry 

 let onlyTiles = 
 VectorArith.GridMath.mathGrids VectorArith.FlexVector.mul combined tilesGrid 

 (* letters,LAMBDA(_,IFS(BITAND(_,8)=8,"E",BITAND(_,4)=4,"N",BITAND(_,2)=2,"W",BITAND(_,1)=1,"S",TRUE,"")), *) 

 let dirTest x y = x land y = y 

 let letters = 
   Agrid.map 
     (fun x -> 
       match x with 
       | x when dirTest x 8 -> "E" 
       | x when dirTest x 4 -> "N" 
       | x when dirTest x 2 -> "W" 
       | x when dirTest x 1 -> "S" 
       | _ -> "X") 
     combined 

 (* let esMain2 () =  *)
 (*   Stdlib.print_endline "Hello, World!";  *)
 (*   FunctionsNew.MazePrinter.print_mazeNew testmaze;  *)
 (*   (* Stdlib.print_endline "Cell Type"; *)  *)
 (*   (* DebugUtil.ppGrid newgrid Stdlib.Format.pp_print_int; *)  *)
 (*   Stdlib.print_endline "Walls";  *)
 (*   DebugUtil.ppGrid wallsGrid Stdlib.Format.pp_print_int;  *)
 (*   Stdlib.print_endline "Tiles";  *)
 (*   DebugUtil.ppGrid tilesGrid Stdlib.Format.pp_print_int;  *)
 (*   Stdlib.print_endline "Open Walls";  *)
 (*   DebugUtil.ppGrid openWallsGrid Stdlib.Format.pp_print_int;  *)
 (*   Stdlib.print_endline "Left Open Walls - 2";  *)
 (*   DebugUtil.ppGrid leftOpenWalls Stdlib.Format.pp_print_int;  *)
 (*   Stdlib.print_endline "North Open Walls - 4";  *)
 (*   DebugUtil.ppGrid northOpenWalls Stdlib.Format.pp_print_int;  *)
 (*   Stdlib.print_endline "Right Open Walls - 8";  *)
 (*   DebugUtil.ppGrid rightOpenWalls Stdlib.Format.pp_print_int;  *)
 (*   Stdlib.print_endline "South Open Walls - 1";  *)
 (*   DebugUtil.ppGrid southOpenWalls Stdlib.Format.pp_print_int;  *)
 (*   FunctionsNew.MazePrinter.print_mazeNew testmaze;  *)
 (*   Stdlib.print_endline "Combined";  *)
 (*   DebugUtil.ppGrid combined Stdlib.Format.pp_print_int;  *)
 (*   Stdlib.print_endline "Letters";  *)
 (*   DebugUtil.ppGrid letters Stdlib.Format.pp_print_string  *)

 let printOpenDirWalls = function 
   | West -> DebugUtil.ppGrid leftOpenWalls Stdlib.Format.pp_print_int 
   | North -> DebugUtil.ppGrid northOpenWalls Stdlib.Format.pp_print_int 
   | East -> DebugUtil.ppGrid rightOpenWalls Stdlib.Format.pp_print_int 
     | South -> DebugUtil.ppGrid southOpenWalls Stdlib.Format.pp_print_int; 


 let maze = FunctionsNew.Generator.binary_tree_maze 10 5  

 let grid = Agrid.of_array maze.grid  
   let mWidth = Agrid.width grid  
   let mHeight = Agrid.height grid  
   let wallsGrid = (Flex_array.make mHeight (Flex_array.make mWidth 0))  
   let tilesGrid = (Flex_array.make mHeight (Flex_array.make mWidth 0))  
   let openWallsGrid = (Flex_array.make mHeight (Flex_array.make mWidth 0))  
   let wallsGrid, tilesGrid, openWallsGrid, _, _ = 
     Agrid.fold 
       (fun (wg, tg, owg, x, y) i -> 
         match checkCell x y i with 
         | 2 -> 
             let () = Stdlib.Printf.printf "Wall (%d, %d)\n" x y  
             (Agrid.set wg ~x ~y 1, tg, owg, incX x, incY y x) 
         | 1 -> 
             let () = Stdlib.Printf.printf "Tile (%d, %d)\n" x y  
             (wg, Agrid.set tg ~x ~y 1, owg, incX x, incY y x) 
         | 0 -> 
             let () = Stdlib.Printf.printf "Open Wall (%d, %d)\n" x y  
             (wg, tg, Agrid.set owg ~x ~y 1, incX x, incY y x) 
         | _ -> (wg, tg, owg, incX x, incY y x)) 
       (wallsGrid, tilesGrid, openWallsGrid, 0, 0) 
       grid 


   let emptyCol = Flex_array.make mHeight 0 in 
   let emptyRow = Flex_array.make mWidth 0 in 
   let leftOpenWalls = Agrid.liat_col (Agrid.cons_col emptyCol openWallsGrid)  
   let northOpenWalls = Agrid.liat_row (Agrid.cons_row emptyRow openWallsGrid)  
   let rightOpenWalls = Agrid.tail_col (Agrid.snoc_col openWallsGrid emptyCol)  
   let southOpenWalls = Agrid.tail_row (Agrid.snoc_row openWallsGrid emptyRow)  
   let leftBit = Agrid.map (fun x -> x * 2) leftOpenWalls  
   let rightBit = Agrid.map (fun x -> x * 8) rightOpenWalls  
   let northBit = Agrid.map (fun x -> x * 4) northOpenWalls  
   let southBit = Agrid.map (fun x -> x * 1) southOpenWalls  
   let gridAry = Flex_array.of_list [ leftBit; rightBit; northBit; southBit ]  
   let combined = VectorArith.GridMath.sumGridArray gridAry  
   let onlyTiles = 
     VectorArith.GridMath.mathGrids VectorArith.FlexVector.mul combined tilesGrid 

   let dirTest x y = x land y = y  
   let letters = 
     Agrid.map 
       (fun x -> 
         match x with 
         | x when dirTest x 8 -> "E" 
         | x when dirTest x 4 -> "N" 
         | x when dirTest x 2 -> "W" 
         | x when dirTest x 1 -> "S" 
         | _ -> "X") 
     combined; 

 let test_queue () = Queue.create ~capacity:10 

 (* let () = Queue.enqueue test_queue step1 *) 
 (*   Queue.enqueue test_queue step2; *) 
 (*     Queue.enqueue test_queue step3; *) 
 (*     Queue.enqueue test_queue step4; *) 
 (*     Queue.enqueue test_queue step5; *) 
 (*     Queue.enqueue test_queue step6; *) 
 (*     Queue.enqueue test_queue step7; *) 
 (*     Queue.enqueue test_queue step8; *) 
 (*     Queue.enqueue test_queue step9; *) 
 (*     Queue.enqueue test_queue step10; *) 
 (*     Queue.enqueue test_queue step11 *) 
 (*  *) 


 let step1  = FunctionsNew.MazePrinter.print_mazeNew testmaze;  
 let step2 () = DebugUtil.ppGrid wallsGrid Stdlib.Format.pp_print_int;  
   let step3 () = 
     Stdlib.print_endline "Walls"; 
 DebugUtil.ppGrid wallsGrid Stdlib.Format.pp_print_int; 


   let step4 () = 
     Stdlib.print_endline "Tiles"; 
 DebugUtil.ppGrid tilesGrid Stdlib.Format.pp_print_int; 

   let step5 () = 
     Stdlib.print_endline "Open Walls"; 
 DebugUtil.ppGrid openWallsGrid Stdlib.Format.pp_print_int; 

   let step6 () = 
     Stdlib.print_endline "Left Open Walls - 2"; 
 DebugUtil.ppGrid leftOpenWalls Stdlib.Format.pp_print_int; 

   let step7 () = 
     Stdlib.print_endline "North Open Walls - 4"; 
 DebugUtil.ppGrid northOpenWalls Stdlib.Format.pp_print_int; 

   let step8 () = 
     Stdlib.print_endline "Right Open Walls - 8"; 
 DebugUtil.ppGrid rightOpenWalls Stdlib.Format.pp_print_int; 

   let step9 () = 
     Stdlib.print_endline "South Open Walls - 1"; 
 DebugUtil.ppGrid southOpenWalls Stdlib.Format.pp_print_int; 

   let step10 () = 
     FunctionsNew.MazePrinter.print_mazeNew testmaze  
     (* Stdlib.print_endline "Combined"; *) 
 DebugUtil.ppGrid combined Stdlib.Format.pp_print_int; 
   (**) 
   (* let step11 = *) 
 (* Stdlib.print_endline "Letters"; *) 
     (* DebugUtil.ppGrid (letters : string Agrid.t) Stdlib.Format.pp_print_string; *) 


 (* let debugSteps = *) 
 (*   [ *) 
 (*     step1; *) 
 (*     step2; *) 
 (*     step3; *) 
 (*     step4; *) 
 (*     step5; *) 
 (*     step6; *) 
 (*     step7; *) 
 (*     step8; *) 
 (*     step9; *) 
 (*     step10; *) 
 (*     step11; *) 
 (*   ] *) 
 (**) 

 (* let step 12 () =  *) 
 (* Stdlib.print_endline "Cell Type"; *) 
 (* DebugUtil.ppGrid newgrid Stdlib.Format.pp_print_int*) 

 (* letters,LAMBDA(_,IFS(BITAND(_,8)=8,"E",BITAND(_,4)=4,"N",BITAND(_,2)=2,"W",BITAND(_,1)=1,"S",TRUE,"")), *) 

 (* Stdlib.print_endline "Left Open Walls - 2"; *) 
 (* DebugUtil.ppGrid leftOpenWalls Stdlib.Format.pp_print_int; *) 
 (* Stdlib.print_endline "North Open Walls - 4"; *) 
 (* DebugUtil.ppGrid northOpenWalls Stdlib.Format.pp_print_int; *) 
 (* Stdlib.print_endline "Right Open Walls - 8"; *) 
 (* DebugUtil.ppGrid rightOpenWalls Stdlib.Format.pp_print_int; *) 
 (* Stdlib.print_endline "South Open Walls - 1"; *) 
 (* DebugUtil.ppGrid southOpenWalls Stdlib.Format.pp_print_int; *) 
 (* FunctionsNew.MazePrinter.print_mazeNew testmaze; *) 
 (* Stdlib.print_endline "Combined"; *) 
 (* DebugUtil.ppGrid combined Stdlib.Format.pp_print_int; *) 
 (* Stdlib.print_endline "Letters"; *) 
 (* DebugUtil.ppGrid letters Stdlib.Format.pp_print_string *) 
