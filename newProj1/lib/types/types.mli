type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree;;

type 'a ntree = 
  | Nleaf
  | Ntree of 'a * 'a ntree list;;

type nestedStringListelement =
  | Str of string
  | List of nestedStringListelement list


