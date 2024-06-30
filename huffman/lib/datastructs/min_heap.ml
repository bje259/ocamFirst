(* min_heap.ml *)
type 'a priority_queue = {
  mutable heap: ('a * int) array;  (* The array storing the heap elements *)
  mutable size: int;               (* The number of elements in the heap *)
}

let create_capacity = 10

let create_priority_queue () = {
  heap = Array.make create_capacity (Obj.magic 0, max_int);  (* Initial array with dummy values *)
  size = 0;  (* Initial size is 0 *)
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

(* Insert an element into the heap *)
let insert pq (element, priority) =
  (* Resize the array if needed *)
  if pq.size = Array.length pq.heap then
    pq.heap <- Array.append pq.heap (Array.make pq.size (Obj.magic 0, max_int));
  
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


let print_heap pq =
  for i = 0 to pq.size - 1 do
    Printf.printf "Value: %s Priority: %d\n" (fst pq.heap.(i)) (snd pq.heap.(i))
  done;
  print_newline ()

let print_in_priority_order pq =
  let pq_copy = { heap = Array.copy pq.heap; size = pq.size } in
  let rec extract_all () =
    if pq_copy.size > 0 then begin
      let min_element = extract_min pq_copy in
      Printf.printf "Value: %s Priority: %d\n" (fst min_element) (snd min_element);
      extract_all ()
    end
  in
  extract_all ();
  print_endline ""

let print_queue pq = print_in_priority_order pq;;

