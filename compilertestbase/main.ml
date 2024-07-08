let rec sum lst = match lst with [] -> 0 | x :: xs -> x + sum xs
let my_list = [ 1; 2; 3; 4; 5 ]
let result = sum my_list
