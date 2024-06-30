open Datastructs.CharHashCount
open Datastructs.Min_heap_gen

(*open DebugM.DebugUtil*)



let original_string = "Hello, Worlds, what a day";;

let () = 
  print_endline "Original string:";
  Printf.printf "%s\n" original_string;;



let seqA = String.to_seq original_string;;

let freq = count_chars seqA;;

let default_node = {
  character = None;
  frequency = 0;
  left = None;
  right = None;
}

(* Initial elements *)
let initElements = List.map (fun (c, f) ->  
 Some {
    character = Some c;
    frequency = f;
    left = None;
    right = None;
  }) freq

(*let pq: 'a priority_queue = create_priority_queue ();;*)
let pq = create_priority_queue ();;


let insert_node pq node = 
  match node with
  | Some n -> 
      (*Printf.printf "Inserting node: %s\n" (string_of_nodeRec (Some n)); *)
      Datastructs.Min_heap_gen.insert pq (n, n.frequency)
  | None ->() ;; 
      (*print_endline "None";;*)

(*let insert_node pq node =*)
  (*match node with*)
  (*| Some n ->*)
      (*(Printf.printf "Inserting node: %s\n" (string_of_nodeRec node); *)
      (*let n2 = Option.value ~default: {character = None; frequency = 0; left = None; right = None} node in*)
      (*Datastructs.Min_heap_gen.insert pq (n2, n2.frequency));*)
  (*| None -> print_endline "None";*)





(*Printf.printf "initElements length: %d\n" (List.length initElements);;*)


List.iter (insert_node pq) initElements;;

 (*Additional debug statement to check the state of the heap *)
(*let printTree = function*)
(*  |Some pq -> print_in_priority_order pq; *)
(*  |None -> ();;*)
  (*|Some pq -> print_in_priority_order (Some pq);*)


(*let () = printTree pq ;;*)
(* *)
(*let print_in_priority_order_prep = function*)
(*  | Some p -> print_in_priority_order p ;*)
(*  | None -> ();;*)

let pqOpt = Some pq;;

(*let () = (print_in_priority_order pq ;*)
(*  (print_endline (string_of_int pq.size)));;*)

let huffTree prq= 
  let build = Datastructs.Min_heap_gen.buildTree prq in 
  build;;



(*let () =*)
(*  Printf.printf "Heap size after insertions: %d\n" pq.size;*)
(*  Array.get pq.heap 0 |>  fun x -> Printf.printf "Current element at 0: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 1 |>  fun x -> Printf.printf "Current element at 1: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 2 |>  fun x -> Printf.printf "Current element at 2: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 3 |>  fun x -> Printf.printf "Current element at 3: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 4 |>  fun x -> Printf.printf "Current element at 4: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 5 |>  fun x -> Printf.printf "Current element at 5: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 6 |>  fun x -> Printf.printf "Current element at 6: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 7 |>  fun x -> Printf.printf "Current element at 7: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 8 |>  fun x -> Printf.printf "Current element at 8: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 9 |>  fun x -> Printf.printf "Current element at 9: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 10 |>  fun x -> Printf.printf "Current element at 10: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 11 |>  fun x -> Printf.printf "Current element at 11: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 12 |>  fun x -> Printf.printf "Current element at 12: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 13 |>  fun x -> Printf.printf "Current element at 13: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*  Array.get pq.heap 14 |>  fun x -> Printf.printf "Current element at 14: %s\n" ( string_of_nodeRec (Some (fst x)));*)
(*;;*)
  (*Array.get pq.heap 15 |>  fun x -> Printf.printf "Current element at 15: %s\n" ( string_of_nodeRec (Some (fst x)));*)
  (*Array.get pq.heap 0 |>  fun x -> Printf.printf "Current element at 0: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 1 |>  fun x -> Printf.printf "Current element at 1: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 2 |>  fun x -> Printf.printf "Current element at 2: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 3 |>  fun x -> Printf.printf "Current element at 3: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 4 |>  fun x -> Printf.printf "Current element at 4: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 5 |>  fun x -> Printf.printf "Current element at 5: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 6 |>  fun x -> Printf.printf "Current element at 6: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 7 |>  fun x -> Printf.printf "Current element at 7: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 8 |>  fun x -> Printf.printf "Current element at 8: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 9 |>  fun x -> Printf.printf "Current element at 9: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 10 |>  fun x -> Printf.printf "Current element at 10: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 11 |>  fun x -> Printf.printf "Current element at 11: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 12 |>  fun x -> Printf.printf "Current element at 12: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 13 |>  fun x -> Printf.printf "Current element at 13: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 14 |>  fun x -> Printf.printf "Current element at 14: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.get pq.heap 15 |>  fun x -> Printf.printf "Current element at 15: %s\n" ( (match (fst x) with | Some x ->  string_of_nodeRec x | None -> "" ) );*)
  (*Array.iteri (fun i (element, priority) ->*)
  (*  Printf.printf "Heap[%d]: %s, priority: %d\n" i (string_of_nodeRec (Some element)) priority*)
  (* ) pq.heap;;*)

(* Insert initial elements into the priority queue *)
(*List.iter (fun x -> Datastructs.Min_heap_gen.insert pq ( x,( Option.value ~defaults: 1 x.frequency ) ) initElements )*)
(*print_queue pq*)

(*let huffTreeBuilt = huffTree pq;;*)

let huff = 
let huffTreeBuilt = huffTree pq in
(*let () = Seq.iter (fun x -> Printf.printf "%c" x) seqA in*)
(*let () =  print_newline () in*)
(*let () = List.iter (fun x -> Printf.printf "%s " (string_of_tuple (string_of_char) (string_of_int) x)) freq in*)
huffTreeBuilt;;




let () = 
  (*Printf.printf "Testing huffTree\n";*)
  (*print_endline ("huff printout: " ^ string_of_nodeRec (Some huff));*)
  (*print_endline ("check first char: " ^ (string_of_char (Option.value ~default:'\000' huff.character))));*)
  (*print_endline ("check first freq: " ^ (string_of_int (  huff.frequency)));*)
  (*print_endline ("array length: " ^ string_of_int (Array.length pq.heap) ^ " Size prop: " ^ string_of_int (pq.size));;*)
  (*Datastructs.Min_heap_gen.print_in_priority_order huff;;*)
  printTreeVis3 (Some huff);;

let codes = generate_codes huff "" [];;
let () = (print_endline "Generated codes\n"; print_codes codes) in
  let encoded_str = encode_string codes original_string in
  Printf.printf "Encoded: %s\n" encoded_str;

(* Decode the string *)
  let decoded_str = decode_string huff encoded_str in
  Printf.printf "Decoded: %s\n" decoded_str




(* Create nodes for testing *)
(*let node1 = { character = Some 'a'; frequency = 5; left = None; right = None }*)
(*let node2 = { character = Some 'b'; frequency = 3; left = None; right = None }*)
(*let node3 = { character = None; frequency = 8; left = Some node1; right = Some node2 }*)
(**)
(*(* Example tree *)*)
(*let example_tree = Some node3*)
(**)
(*(* Print the tree *)*)





(*let () =  *)
(*  let pq = create_priority_queue () in*)
(*  Printf.printf "Inserting %s with priority %d\n" "task1" 5;*)
(*  insert pq ("task1",5);*)
(*  Printf.printf "Inserting %s with priority %d\n" "task2" 1;*)
(*  insert pq ("task2",1);                                 *)
(*  Printf.printf "Inserting %s with priority %d\n" "task3" 3;*)
(*  insert pq ("task3",3);                                 *)
(*  Printf.printf "Inserting %s with priority %d\n" "task4" 10;*)
(*  insert pq ("task4",10);*)
(*  (*Printf.printf "Min element: %s\n" (peek pq);*)*)
(*  (*Printf.printf "Extracted min: %s\n" (extract_min pq);*)*)
(*  (*Printf.printf "Min element: %s\n" (peek pq)*)*)
(*  let (value,priority) = peek pq in*)
(*  Printf.printf "Min element: %s with prio: %d\n" value priority; *)
(*  let (value,priority)  = extract_min pq in*)
(*  Printf.printf "Extracted min: %s with prio: %d\n" value priority; *)
(*  let (value,priority)  = peek pq in*)
(*  Printf.printf "Min element: %s with prio: %d\n" value priority; *)
(*  print_queue pq;*)
(*  let (value,priority)  = extract_min pq in*)
(*  Printf.printf "Extracted min: %s with prio: %d\n" value priority; *)
(*  Printf.printf "Inserting %s with priority %d\n" "task5" 2;*)
(*  insert pq ("task5", 2);*)
(*  print_queue pq;*)
(*  let (value,priority)  = extract_min pq in*)
(*  Printf.printf "Extracted min: %s with prio: %d\n" value priority; *)
(*  print_queue pq;*)
(*  let (value,priority)  = extract_min pq in*)
(*  Printf.printf "Extracted min: %s with prio: %d\n" value priority; *)
(*  print_queue pq;*)
