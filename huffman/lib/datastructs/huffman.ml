module Char_tbl = Hashtbl.Make(struct
  type t = char
  let equal = Char.equal
  let hash = Hashtbl.hash
end)



