open Notty
open Notty_unix

(* open Lib.Gamesetting *)
open Agrid
open Flex_array

(* open Lib.PpConway *)
(* open Lib.Gamesetting2 *)
(* open Lib.Gamesetting2 *)

(* let dp str = Ocamlutils.DebugUtil.Printing.print_string str *)
(**)
(* let agFoldi f acc (grid : 'a t) = *)
(*   Flex_array.foldi *)
(*     (fun acc y row -> Flex_array.foldi (fun acc x v -> f ~x ~y acc v) acc row) *)
(*     acc grid *)

(* let test_grid = Agrid.of_array [| [| 1; 2; 3 |]; [| 4; 5; 6 |]; [| 7; 8; 9 |] |] *)
(**)
(* let sum_indices_and_values ~x ~y acc v = *)
(*   let () = dp (Printf.sprintf "x: %d, y: %d, acc: %d, v: %d\n" x y acc v) in *)
(*   acc + x + y + v *)
(**)
(* let result = agFoldi sum_indices_and_values 0 test_grid *)
(**)
(* let () = Printf.printf "The result is: %d\n" result *)

(* let initial_grid = *)
(*   let width = 10 in *)
(*   let height = 10 in *)
(*   initgrid ~width ~height () *)
(**)
(* let nottyPPGrid grid = *)
(*   let width = Agrid.width grid in *)
(*   let height = Agrid.height grid in *)
(*   let img = I.void width height in *)
(*   let img = agFoldi *)
(*       (fun ~x ~y img cell -> *)
(*             let cellImg = I.(string A.empty (string_of_cell cell) |> hpad (x*2) 0 |> vpad y 0 ) in *)
(*             let img = I.(img </> cellImg) in *)
(*         img) *)
(*        img grid *)
(*   in *)
(*   img *)

let () = Lib.Loop_event.main ()
(*   let t = Term.create () in *)
(*   let a1 = A.(fg lightwhite) in *)
(*   let a2 = A.(fg red) in *)
(*   let img1 = I.string a1 "Hello" in *)
(*   let img2 = I.string a2 " World!" in *)
(*   (* Combine images horizontally *) *)
(*   let combined_h = I.(img1 <|> img2) in *)
(*   (* Combine images vertically *) *)
(*   let combined_v = I.(img1 <-> img2) in *)
(*   (* Display the horizontally combined image *) *)
(*     Term.image t (nottyPPGrid initial_grid); *)
(* (* Wait for a key press *) *)
(*   (* Wait for a key press *) *)
(*   let _ = Term.event t in *)
(*   (* Display the vertically combined image *) *)
(*   Term.image t combined_v; *)
(*   (* Wait for another key press before exiting *) *)
(*   let _ = Term.event t in *)
(*   Term.release t *)
(**)
(* let square = "\xe2\x96\xaa" *)

(* let rec sierp n = *)
(*   if n > 1 then ( *)
(*     let ss = sierp (pred n) in *)
(*     I.(ss <-> (ss <|> ss))) *)
(*   else *)
(* I.(string A.(fg magenta) square |> hpad 1 0) *)
(* ;; *)

(* let rec sierp n = *)
(*   if n > 1 then ( *)
(*     let ss = sierp (n - 1) in *)
(*     I.(ss <-> (ss <|> ss))) *)
(*   else *)
(*     I.(string A.(fg magenta) square |> hpad 1 0) *)
(* ;; *)
(**)
(* let img (double, n) = *)
(*   let s = sierp n in *)
(*   if double then *)
(*     I.(s </> vpad 1 0 s) *)
(*   else *)
(*     s *)
(* ;; *)
(**)
(* let rec update t state = *)
(*   Term.image t (img state); *)
(*   loop t state *)
(**)
(* and loop t ((double, n) as state) = *)
(*   match Term.event t with *)
(*   | `Key (`Enter, _) -> () *)
(*   | `Key (`Arrow `Left, _) -> update t (double, max 1 (n - 1)) *)
(*   | `Key (`Arrow `Right, _) -> update t (double, min 8 (n + 1)) *)
(*   | `Key (`ASCII ' ', _) -> update t (not double, n) *)
(*   | `Resize _ -> update t state *)
(*   | _ -> loop t state *)
(* ;; *)
(**)
(* let () = *)
(*   let t = Term.create () in *)
(*   update t (false, 1); *)
(*   Term.release t *)
(* ;; *)
