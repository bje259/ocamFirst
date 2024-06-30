type 'a node = {
  value: 'a;
  priority: int;
  mutable next: 'a node option;
}

type 'a priority_queue = {
  mutable head: 'a node option;
}

let create_priority_queue () = { head = None }

let insert pq value priority =
  let new_node = { value; priority; next = None } in
  let rec insert_aux = function
    | None -> Some new_node
    | Some node as head ->
      if priority < node.priority then begin
        new_node.next <- head;
        Some new_node
      end else begin
        node.next <- insert_aux node.next;
        head
      end
  in
  pq.head <- insert_aux pq.head

let extract_min pq =
  match pq.head with
  | None -> failwith "Priority queue is empty"
  | Some node ->
    pq.head <- node.next;
    node

let peek pq =
  match pq.head with
  | None -> failwith "Priority queue is empty"
  | Some node -> node


let print_queue pq =
  let rec print_aux = function
    | None -> ()
    | Some node ->
      Printf.printf "%d " node.priority;
      print_aux node.next
  in
  print_aux pq.head;
  print_newline ();;





