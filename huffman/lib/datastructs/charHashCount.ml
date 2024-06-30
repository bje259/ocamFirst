module Char_tbl = Hashtbl.Make(struct
  type t = char
  let equal = Char.equal
  let hash = Hashtbl.hash
end)

(*  count distinct occurrences of chars in [seq] *)
let count_chars (seq : char Seq.t) : _ list =
  let counts = Char_tbl.create 16 in
    Seq.iter
      (fun c ->
        let count_c =
          Char_tbl.find_opt counts c
          |> Option.value ~default:0
          in
            Char_tbl.replace counts c (count_c + 1))
      seq;
      (* turn into a list *)
      Char_tbl.fold (fun c n l -> (c,n) :: l) counts []
      |> List.sort (fun (c1,_)(c2,_) -> Char.compare c1 c2)





