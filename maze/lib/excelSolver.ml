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

let checkCell (x : int) (y : int) (c : cell) =
  match c with
  | Wall -> 2
  | Path when x mod 2 = 0 && y mod 2 = 0 -> 1
  | Path -> 0
