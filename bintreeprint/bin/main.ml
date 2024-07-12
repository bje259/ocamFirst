open Lib.DebugUtil
open Lib.Types
open Lib.VectorArith
open Core
open Agrid
open Flex_array

let () = print_endline "Hello, World!"

let count = 10

let delimiter = ' '

let printNode nodeValue space f =
  print_string (String.make space delimiter) ;
  print_endline (f nodeValue)

let rec printTreeUtil root space =
  match root with
  | Empty ->
      ()
  | Node (l, v, r) ->
      let newSpace = space + count in
      let () = printTreeUtil l newSpace in
      let () = printNode v newSpace string_of_int in
      let () = printTreeUtil r newSpace in
      ()

let printTree root = printTreeUtil root 0

let t =
  Node
    ( Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))
    , 4
    , Node (Node (Empty, 5, Empty), 6, Node (Empty, 7, Empty)) )

let () = printTree t
