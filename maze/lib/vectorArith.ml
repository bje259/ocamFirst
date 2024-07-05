(* open Types *)
open Base

module Vector = struct
  let add arr1 arr2 =
    if Array.length arr1 <> Array.length arr2 then
      invalid_arg "Arrays must have the same length"
    else
      Array.init ~f:(fun i -> arr1.(i) + arr2.(i)) (Array.length arr1)

  let sub arr1 arr2 =
    if Array.length arr1 <> Array.length arr2 then
      invalid_arg "Arrays must have the same length"
    else
      Array.init ~f:(fun i -> arr1.(i) - arr2.(i)) (Array.length arr1)

  let mul arr1 arr2 =
    if Array.length arr1 <> Array.length arr2 then
      invalid_arg "Arrays must have the same length"
    else
      Array.init ~f:(fun i -> arr1.(i) * arr2.(i)) (Array.length arr1)

  let div arr1 arr2 =
    if Array.length arr1 <> Array.length arr2 then
      invalid_arg "Arrays must have the same length"
    else
      Array.init ~f:(fun i -> arr1.(i) / arr2.(i)) (Array.length arr1)
end

module FlexVector = struct
  let add arr1 arr2 =
    if Flex_array.length arr1 <> Flex_array.length arr2 then
      invalid_arg "Arrays must have the same length"
    else
      Flex_array.init (Flex_array.length arr1) (fun i ->
          Flex_array.get arr1 i + Flex_array.get arr2 i)

  let sub arr1 arr2 =
    if Flex_array.length arr1 <> Flex_array.length arr2 then
      invalid_arg "Arrays must have the same length"
    else
      Flex_array.init (Flex_array.length arr1) (fun i ->
          Flex_array.get arr1 i - Flex_array.get arr2 i)

  let mul arr1 arr2 =
    if Flex_array.length arr1 <> Flex_array.length arr2 then
      invalid_arg "Arrays must have the same length"
    else
      Flex_array.init (Flex_array.length arr1) (fun i ->
          Flex_array.get arr1 i * Flex_array.get arr2 i)

  let div arr1 arr2 =
    if Flex_array.length arr1 <> Flex_array.length arr2 then
      invalid_arg "Arrays must have the same length"
    else
      Flex_array.init (Flex_array.length arr1) (fun i ->
          Flex_array.get arr1 i / Flex_array.get arr2 i)
end

module GridMath = struct
  let mathGrids f arr1 arr2 =
    let height1 = Agrid.height arr1 in
    let width1 = Agrid.width arr1 in
    let height2 = Agrid.height arr2 in
    let width2 = Agrid.width arr2 in
    if height1 <> height2 || width1 <> width2 then
      invalid_arg "Arrays must have the same dimensions"
    else
      let rec aux arr1 arr2 acc =
        let v1 = Agrid.get_col arr1 0 in
        let v2 = Agrid.get_col arr2 0 in
        let newCol = f v1 v2 in
        let newAcc = Agrid.snoc_col acc newCol in
        if Agrid.width arr1 = 1 then
          newAcc
        else
          aux (Agrid.tail_col arr1) (Agrid.tail_col arr2) newAcc
      in
      aux arr1 arr2 Agrid.empty

  let sumGridArray arr =
    let height = Agrid.height (Flex_array.get arr 0) in
    let width = Agrid.width (Flex_array.get arr 0) in
    let dimCheck =
      Flex_array.fold
        (fun acc x ->
          if Agrid.height x = height && Agrid.width x = width then
            acc
          else
            false)
        true arr
    in
    match dimCheck with
    | false -> invalid_arg "Arrays must have the same dimensions"
    | true ->
        let sumGrid =
          Flex_array.fold
            (fun acc x -> mathGrids FlexVector.add acc x)
            (Flex_array.get arr 0) (Flex_array.tail arr)
        in
        let () = DebugUtil.ppGrid sumGrid Stdlib.Format.pp_print_int in
        sumGrid
end
