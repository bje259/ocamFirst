(* min_heap_gen.ml *)

open DebugM.DebugUtil

(*let with_priority = function*)
(*  | None -> failwith "None"*)
(*  | Some x -> (x,(Option.value ~default:0 x.frequency))*)


type 'a priority_queue = {
  mutable heap: ('a * int) array;
  mutable size: int;
}

type nodeRec = {
  character: char option;
  frequency: int;
  left: nodeRec option;
  right: nodeRec option;
}
(* Define a default node for use when left or right is None *)
let default_node = { character = None; frequency = 0; left = None; right = None }


let rec string_of_nodeRec node =
  match node with
  | None -> "None"
  | Some n ->
      "{\n" ^
      "\tcharacter: " ^ string_of_char_option n.character ^ ";\n " ^
      "\tfrequency: " ^ string_of_int n.frequency ^ ";\n " ^
      "\tleft: " ^ string_of_nodeRec n.left ^ ";\n " ^
      "\tright: " ^ string_of_nodeRec n.right ^ "\n" ^
      "}\n"

let string_of_nodeRecNO n =
  (*match n.left with*)
  (*| None -> ""*)
  (*| Some nleft -> string_of_nodeRecNO*)
  (*     *)
  
      "{\n" ^
      "\tcharacter: " ^ string_of_char_option n.character ^ ";\n" ^
      "\tfrequency: " ^ string_of_int n.frequency ^ ";\n " ^
      "\tleft: " ^ string_of_nodeRec n.left ^ ";\n " ^
      "\tright: " ^ string_of_nodeRec n.right ^ "\n" ^
      "}\n"

(*type nodeRecOption = nodeRec option*)

(*type efullNode = ('a*int)*)

let create_capacity = 10

(*let create_priority_queue (): 'a priority_queue = {*)
(*  heap = Array.make create_capacity (None,max_int);  (* Initial array with dummy values *)*)
(*  size = 0;  (* Initial size is 0 *)*)
(*}*)
(* Create a priority queue *)
(*let create_priority_queue () = {*)
(*  heap = Array.make 16 (Obj.magic (), 0);  (* Initial capacity with dummy values *)*)
(*  size = 0;*)
(*}*)
(* Create a priority queue *)
let create_priority_queue () = {
  heap = Array.make 16 (Obj.magic (), 0);  (* Initial capacity with dummy values *)
  size = 0;
}

(* Helper functions to calculate parent and children indices *)
let parent i = (i - 1) / 2
let left_child i = (2 * i) + 1
let right_child i = (2 * i) + 2

(* Swap elements in the heap *)
let swap pq i j =
  let temp = pq.heap.(i) in
  pq.heap.(i) <- pq.heap.(j);
  pq.heap.(j) <- temp

(* Resize the array *)
let resize pq =
  let new_size = max 1 (2 * Array.length pq.heap) in
  let new_heap = Array.make new_size (Obj.magic (), 0) in
  Array.blit pq.heap 0 new_heap 0 pq.size;
  pq.heap <- new_heap

  (* Insert an element into the heap *)
    let insert pq (element, priority) =
  (* Resize the array if needed *)
  if pq.size = Array.length pq.heap then resize pq;
  
  (* Add the new element at the end of the heap *)
  let i = pq.size in
  pq.size <- pq.size + 1;
    pq.heap.(i) <- (element, priority);

  (* Percolate up to maintain the heap property *)
  let rec percolate_up i =
    if i > 0 && snd pq.heap.(i) < snd pq.heap.(parent i) then begin
      swap pq i (parent i);
      percolate_up (parent i)
    end
  in
  percolate_up i


(* Remove and return the element with the highest priority (smallest element) *)
let extract_min pq =
  if pq.size = 0 then failwith "Priority queue is empty";
  let min_element = pq.heap.(0) in
  pq.size <- pq.size - 1;
  pq.heap.(0) <- pq.heap.(pq.size);

  (* Percolate down to maintain the heap property *)
  let rec percolate_down i =
    let smallest = ref i in
    let l = left_child i in
    let r = right_child i in
    if l < pq.size && snd pq.heap.(l) < snd pq.heap.(!smallest) then
      smallest := l;
    if r < pq.size && snd pq.heap.(r) < snd pq.heap.(!smallest) then
      smallest := r;
    if !smallest <> i then begin
      swap pq i !smallest;
      percolate_down !smallest
    end
  in
  percolate_down 0;
  min_element

(* Peek at the element with the highest priority (smallest element) without removing it *)
let peek pq =
  if pq.size = 0 then failwith "Priority queue is empty";
  pq.heap.(0)


(*let print_heap pq =*)
(*  for i = 0 to pq.size - 1 do*)
(*    Printf.printf "Value: %s Priority: %d\n" (fst pq.heap.(i)) (snd pq.heap.(i))*)
(*  done;*)
(*  print_newline ()*)

(*let print_in_priority_order pq =*)
(*(*let pq_copy = { heap = Array.copy pq.heap; size = pq.size } in*)*)
(*  let pq_copy = { heap = Array.copy pq.heap; size = pq.size } in*)
(*  let rec extract_all () =*)
(*    if pq_copy.size > 0 then begin*)
(*      let min_element = extract_min pq_copy in*)
(*      let x = min_element in*)
(*      Printf.printf "Current element: %s\n"  (string_of_nodeRec (fst ( x ) )) ; (*string_of_nodeRec (fst min_element)*)*)
(*      (*Printf.printf "Current element: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) ); (*string_of_nodeRec (fst min_element)*)*)*)
(*      extract_all ();*)
(*    end*)
(*  in*)
(*  extract_all ();*)
(*  print_endline ""*)

(*let print_in_priority_order pq =*)
(*  let pq_copy = { heap = Array.copy pq.heap; size = pq.size } in*)
(*  let rec extract_all () =*)
(*    if pq_copy.size > 0 then begin*)
(*      let min_element = (extract_min pq_copy) in*)
(*      match fst min_element with*)
(*      | Some x -> Printf.printf "Current element: %s\n" (string_of_nodeRec (Some x))*)
(*      | None -> print_endline "None";*)
(*      extract_all ()*)
(*    end*)
(*  in*)
(*  extract_all ();*)
(*  print_endline ""(*let print_queue pq = print_in_priority_order pq*)*)

let print_in_priority_order pq =
  let pq_copy = { heap = Array.copy pq.heap; size = pq.size } in
  let rec extract_all () =
    if pq_copy.size > 0 then begin
      let min_element = (extract_min pq_copy) in
      let str_min =  string_of_nodeRecNO (fst min_element) ^ "\n" in
      let () = Printf.printf "Current element: %s\n" str_min in
      extract_all ()
    end
  in
  extract_all ();
  print_endline ""(*let print_queue pq = print_in_priority_order pq*)


let buildTree pqi = 
  (*let pq_copy = { heap = Array.copy pq.heap; size = pq.size } in*)
  (*type 'a = ('a,*'a pqTree*'a pqTree) in*)
  (*let pq_copy = { heap = Array.copy pq.heap; size = pq.size } in*)
  let rec buildTreeAux (pq:nodeRec priority_queue) =
    if pq.size = 1 then
     fst pq.heap.(0)
    else
      let (lft,lfreq) = extract_min pq in
      let (rght,rfreq) = extract_min pq in
      let newFreq = lfreq + rfreq in
      let newNode: nodeRec = {character = None; frequency = newFreq; left = Some lft; right = Some rght} in
      (*let newNodeOpt: nodeRec = newNode in*)
      insert pq ( newNode, newFreq);
      buildTreeAux pq;
  in buildTreeAux pqi ;;

let describe_tree2 node =
  match node with
  | { character = Some c; frequency = f; left = Some _; right = None } ->
      Printf.sprintf "-%c-%d-" c f
  | { character = Some c; frequency = f; left = None; right = Some _ } ->
      Printf.sprintf  "-%c-%d-" c f
  | { character = Some c; frequency = f; left = Some _; right = Some _ } ->
      Printf.sprintf "-%c-%d-" c f
  | { character = Some c; frequency = f; left = None; right = None } ->
      Printf.sprintf "-%c-%d-" c f
  | { character = None; _ } ->
     Printf.sprintf "-I-" 

(*let printTreeVis tree =*)
(*  let rec aux prefix is_last cur = *)
(*      match fst cur with*)
(*      | { character = Some _; frequency = _; left = l; right = r } ->*)
(*        let left_node = match l with Some ln -> ln | None -> default_node in*)
(*        let right_node = match r with Some rn -> rn | None -> default_node in*)
(*        let current = Printf.sprintf "%s%s%s\n" prefix (if is_last then "└── " else "├── ") (describe_tree2 (fst cur)) in*)
(*        let new_prefix = prefix ^ (if is_last then "    " else "│   ") in*)
(*        let left = aux new_prefix (right_node=None) left_node  in*)
(*        let right = aux new_prefix true right_node in*)
(*        current ^ left ^ right*)
(*      | { character = None; frequency = _; left = l; right = r } -> *)
(*        let left_node = match l with Some ln -> ln | None -> default_node in*)
(*        let right_node = match r with Some rn -> rn | None -> default_node in*)
(*        let current = Printf.sprintf "%s%s%s\n" prefix (if is_last then "└── " else "├── ") (describe_tree2 (fst cur)) in*)
(*        let new_prefix = prefix ^ (if is_last then "    " else "│   ") in*)
(*        let left = aux new_prefix (right_node=None) left_node in*)
(*        let right = aux new_prefix true right_node in*)
(*        current ^ left ^ right*)
(*  in print_endline (aux "" true tree)*)

      
(* Utility function to convert a nodeRec to a string *)
let string_of_nodeRec2 node =
  let char_str = match node.character with
    | Some c -> String.make 1 c
    | None -> "None"
  in
  Printf.sprintf "{character: %s, frequency: %d}" char_str node.frequency(* Utility function to convert a nodeRec to a string *)


(* Printing function for the tree *)
let printTreeVis2 tree =
  let rec aux prefix is_last node =
    let current = Printf.sprintf "%s%s%s\n" prefix (if is_last then "└── " else "├── ") (string_of_nodeRec2 node) in
    let new_prefix = prefix ^ (if is_last then "    " else "│   ") in
    let left = match node.left with
      | Some l -> aux new_prefix false l
      | None -> ""
    in
    let right = match node.right with
      | Some r -> aux new_prefix true r
      | None -> ""
    in
    current ^ left ^ right
  in
  match tree with
  | Some root -> print_endline (aux "" true root)
  | None -> print_endline "Tree is empty"



let string_of_nodeRec3 node =
  let char_str = match node.character with
    | Some c -> Printf.sprintf "'%c'" c
    | None -> "None"
  in
  Printf.sprintf "{character: %s, frequency: %d}" char_str node.frequency



(* Printing function for the tree *)
let printTreeVis3 tree =
  let rec aux prefix is_last node =
    let current = Printf.sprintf "%s%s%s\n" prefix (if is_last then "└── " else "├── ") (string_of_nodeRec3 node) in
    let new_prefix = prefix ^ (if is_last then "    " else "│   ") in
    let left = match node.left with
      | Some l -> aux new_prefix false l
      | None -> ""
    in
    let right = match node.right with
      | Some r -> aux new_prefix true r
      | None -> ""
    in
    current ^ left ^ right
  in
  match tree with
  | Some root -> print_endline (aux "" true root)
  | None -> print_endline "Tree is empty"
let describe_node node =
  match node with
  | { character = Some c; frequency = f; left = _; right = _ } ->
      Printf.sprintf "Character: %c, Frequency: %d" c f
  | { character = None; frequency = f; left = _; right = _ } ->
      Printf.sprintf "No character, Frequency: %d" f


let describe_tree node =
  match node with
  | { character = Some c; frequency = f; left = Some _ ; right = None } ->
      Printf.sprintf "Node with character %c, frequency %d, and a left child" c f
  | { character = Some c; frequency = f; left = None; right = Some _ } ->
      Printf.sprintf "Node with character %c, frequency %d, and a right child" c f
  | { character = Some c; frequency = f; left = Some _ ; right = Some _ } ->
      Printf.sprintf "Node with character %c, frequency %d, and two children" c f
  | { character = Some c; frequency = f; left = None; right = None } ->
      Printf.sprintf "Leaf node with character %c, frequency %d" c f
  | { character = None; _ } ->
      "Internal node with no character"



(* Function to generate Huffman codes *)
let rec generate_codes node prefix acc =
  match node with
  | { character = Some c; _ } -> (c, prefix) :: acc
  | { left = Some l; right = Some r; _ } ->
      let left_acc = generate_codes l (prefix ^ "0") acc in
      generate_codes r (prefix ^ "1") left_acc
  | { left = Some l; right = None; _ } ->
      generate_codes l (prefix ^ "0") acc
  | { left = None; right = Some r; _ } ->
      generate_codes r (prefix ^ "1") acc
  | _ -> acc

(* Function to print Huffman codes *)
let print_codes codes =
  List.iter (fun (c, code) -> Printf.printf "%c: %s\n" c code) codes


(* Function to encode a string using Huffman codes *)
let encode_string codes str =
  let char_to_code c =
    List.assoc c codes
  in
  String.to_seq str
  |> Seq.map char_to_code
  |> List.of_seq
  |> String.concat ""


(* Function to decode a binary string using the Huffman tree *)
let decode_string tree encoded_str =
  let rec aux node bits =
    match node, bits with
    | { character = Some c; _ }, [] -> [c]
    | { character = Some c; _ }, _ -> c :: aux tree bits
    | { left = Some l; _ }, '0' :: rest -> aux l rest
    | { right = Some r; _ }, '1' :: rest -> aux r rest
    | _ -> failwith "Invalid encoded string"
  in
  aux tree (String.to_seq encoded_str |> List.of_seq)
  |> List.map (String.make 1)
  |> String.concat ""




