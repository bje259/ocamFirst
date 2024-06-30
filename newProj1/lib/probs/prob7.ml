open Debug;;

(*# count_leaves Empty;;*)
(*- : int = 0*)
open Types;;
(*type Node = Node of 'a * 'a tree * 'a tree;;*)
let rec count_leaves: 'a tree -> 'a = function
  | Leaf -> 1
  | Node (_, Leaf, Leaf) -> 2
  | Node (_, l, r) -> count_leaves l + count_leaves r

(*# count_leaves (Node(1, Leaf, Leaf));;*)
(*- : int = 1*)

let count_leaves3 inTree =
  let rec count_leaves2 acc = function
  | Leaf -> acc + 1
  (*| Node (_, Leaf, Leaf) -> acc + 2*)
  | Node (_, l, r) -> count_leaves2 (count_leaves2 acc l) r
  in count_leaves2 0 inTree
(*# count_leaves2 0 (Node(1, Leaf, Leaf));;*)
(*- : int = 1*)

(* generate some saple tree data *)
  let rec generate_tree depth =
    if depth = 0 then Leaf
    else Node (0, generate_tree (depth - 1), generate_tree (depth - 1))


  let printTree tree =
    let rec printTree2 = function
      | Leaf -> print_endline "Leaf"
      | Node (_, l, r) -> print_endline "Node"; printTree2 l; printTree2 r
    in printTree2 tree

    
  let printTree3 tree =
    let rec treeToList = function
      | Leaf ->["lf"] 
      | Node (v, l, r) -> (string_of_int v ) ::treeToList l @ treeToList r
      in let lst =  treeToList tree in
    List.iter (fun x -> print_endline x) lst

    let printTree4 tree =
      let rec aux prefix is_last = function
        | Leaf -> ""
        | Node (v, l, r) ->
            let current = Printf.sprintf "%s%s%s\n" prefix (if is_last then "└── " else "├── ") (string_of_int v) in
            let new_prefix = prefix ^ (if is_last then "    " else "│   ") in
            let left = aux new_prefix (r=Leaf) l in
            let right = aux new_prefix true r in
            current ^ left ^ right
        in print_endline (aux "" true tree)


    let printTreeChar tree =
      let rec aux prefix is_last = function
        | Leaf -> ""
        | Node (v, l, r) ->
            let current = Printf.sprintf "%s%s%s\n" prefix (if is_last then "└── " else "├── ") (string_of_char v) in
            let new_prefix = prefix ^ (if is_last then "    " else "│   ") in
            let left = aux new_prefix (r=Leaf) l in
            let right = aux new_prefix true r in
            current ^ left ^ right
        in print_endline (aux "" true tree)

    let collect_leaves tree = 
      let rec collect_leaves2 acc = function
      | Leaf -> acc
      | Node (v, Leaf, Leaf) -> v::acc
      | Node (_, l, r) -> collect_leaves2 (collect_leaves2 acc l) r
      in List.rev (collect_leaves2 [] tree)

    let collect_internals tree = 
      let rec collect_internals2 acc = function
      | Leaf -> acc
      | Node (_, Leaf, Leaf) -> acc
      | Node (v, l, r) -> v::(collect_internals2 (collect_internals2 acc l) r)
      in let result = (collect_internals2 [] tree) in
     (List.rev result) @ result

    
    let collect_leavesOrd tree = 
      let rec collect_leaves2 acc = function
      | Leaf -> acc
      | Node (v, Leaf, Leaf) -> v::acc
      | Node (_, l, r) -> collect_leaves2 (collect_leaves2 acc r) l
      in List.rev (collect_leaves2 [] tree)

    let collect_internalsOrd tree = 
      let rec collect_internals3 acc = function
      | Leaf -> acc
      | Node (_, Leaf, Leaf) -> acc
      | Node (v, l, r) ->  collect_internals3 (v::collect_internals3 acc r) l 
      in collect_internals3 [] tree

  (* Having an accumulator acc prevents using inefficient List.append.
   * Every internal node will be pushed directly into accumulator.
   * Not tail-recursive, but that is no problem since we have a binary tree and
   * and stack depth is logarithmic. *)
  let internals t = 
    let rec internals_aux t acc = match t with
      | Leaf -> acc
      | Node (_, Leaf, Leaf) -> acc
      | Node (x, l, r) -> internals_aux l (x :: internals_aux r acc)
    in
    internals_aux t [];;
(*val internals : 'a binary_tree -> 'a list = <fun>*)

  let at_level (t, lev) = 
    let rec at_level_aux (t, lev) acc = match t with
      | Leaf -> acc
      | Node (x, _, _) when lev = 0 -> x :: acc
      | Node (_, l, r) -> at_level_aux (l, (lev-1)) (at_level_aux (r, (lev-1)) acc)
    in
    at_level_aux (t, lev) [];;
  

