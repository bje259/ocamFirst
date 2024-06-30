open Types;;
open Debug;;

(* Define a configuration type *)
type 'a config = {
  stringify: 'a -> string;
  compare: 'a -> 'a -> bool;
  unwrap: 'a option -> 'a;
};;

(* Create configurations for different types of answers *)
let string_config = {
  stringify = (fun x -> x);
  compare = ( = );
  unwrap = (function Some x -> x | None -> "");
}

let string_list_config = {
  stringify = (fun l -> "[" ^ (String.concat "; " l) ^ "]");
  compare = ( = );
  unwrap = (function Some x -> x | None -> []);
}

let int_list_config = {
  stringify = (fun l -> "[" ^ (String.concat "; " (List.map string_of_int l)) ^ "]");
  compare = ( = );
  unwrap = (function Some x -> x | None -> []);
}
let int_list_of_list_config = {
  stringify = (fun l -> "[" ^ (String.concat "; " (List.map (fun x -> "[" ^ (String.concat "; " (List.map string_of_int x)) ^ "]") l)) ^ "]");
  compare = ( = );
  unwrap = (function Some x -> x | None -> []);
}

let string_tuple_config = {
  stringify = (fun (x, y, z) -> "(" ^ x ^ ", " ^ y ^ ", [" ^ (String.concat "; " z) ^ "])");
  compare = ( = );
  unwrap = (function Some x -> x | None -> ("", "", []));
}

let int_tuple_config = {
  stringify = (fun (x, y, z) -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ", [" ^ (String.concat "; " (List.map string_of_int z)) ^ "])");
  compare = ( = );
  unwrap = (function Some x -> x | None -> (0, 0, []));
}
let int_config = {
  stringify = string_of_int;
  compare = ( = );
  unwrap = (function Some x -> x | None -> 0);
}
let int_tree_config = {
  stringify = (fun _ -> "Tree");
  compare = ( = );
  unwrap = (function Some x -> x | None -> 0);
}
let char_tree_config = {
  stringify = (fun _ -> "Tree");
  compare = ( = );
  unwrap = (function Some x -> x | None -> '\000')
}
let char_list_config = {
  stringify = (fun l -> string_of_char_list l );
  compare = ( = );
  unwrap = (function Some x -> x | None -> ['\000']);
}

(* Define the lambda function *)

(* Define the tree type *)

(* Define the lambda function *)



(*type NewProj1.Prob7.tree = (Leaf | Node of 'a * 'a tree * 'a tree);;*)
(* Define the lambda function *)
(*type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;*)

(* Test cases for the function *)

(* the code below constructs this tree:
         4
       /   \
      2     5
     / \   / \
    1   3 6   7
*)
let t =
  Node(4,
    Node(2,
      Node(1, Leaf, Leaf),
      Node(3, Leaf, Leaf)
    ),
    Node(5,
      Node(6, Leaf, Leaf),
      Node(7, Leaf, Leaf)
    )
  )

let exampleTree2  = (Node (4, Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf)), Leaf));;
(* Test cases for the function *)

let example_tree =
  Node ('a', Node ('b', Node ('d', Leaf, Leaf), Node ('e', Leaf, Leaf)),
       Node ('c', Leaf, Node ('f', Node ('g', Leaf, Leaf), Leaf)));;

let example_tree_1 = Ntree ('a', [Ntree ('f', [])])

let example_tree_2 =
  Ntree ('b', [
    Ntree ('c', []);
    Ntree ('d', [
      Ntree ('f', []);
      Ntree ('g', [])
    ]);
    Ntree ('e', [])
  ])

let example_nary_tree =
  Ntree ('a', [
    Ntree ('b', [Nleaf]);
    Ntree ('c', [Ntree ('d', [Nleaf]); Ntree ('e', [Nleaf])])
  ])

let testInputAndAnswers = [
  (((example_tree_1), char_tree_config), (1, int_config));
  (((example_nary_tree), char_tree_config), (6, int_config));
  (((example_tree_2), char_tree_config), (6, int_config));
  (((example_tree_2), char_tree_config), (6, int_config));
]


(* Define the lambda function *)

let lambda = let open Probs.Prob8 in
iplN;;
(* Generalized function to test inputs and compare results *)
;;

(* Generalized function to test inputs and compare results *)
let test_function test_cases = List.iteri (fun i currEle -> 
    let ((currInput, _), (curAnswer, config2)) = currEle in
    let result = lambda currInput in
    let () = Probs.Prob7.printTreeChar currInput in
    let resultcheck = config2.compare result curAnswer in
    (
      Printf.printf "Result for problem %d: %s\n" (i + 1) (config2.stringify result);
      Printf.printf "Expected answer %d is: %s\n" (i + 1) (config2.stringify curAnswer);
      Printf.printf "Result check %d: %b\n" (i + 1) resultcheck;
    )
   ) test_cases;;

let () = test_function testInputAndAnswers ;;

(*let testsNestedStrLst: nestedStringListelement = List [Str "1";Str "2";List [Str "3";Str "4";Str "5";Str "6"];Str "7";Str "8";Str "9"] in*)
(*NewProj1.Debug.debug_print_nested_string_list testsNestedStrLst*)



