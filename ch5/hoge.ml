let f x = print_string "f "; x + 1
let g x = print_string "g "; x * 2

let a =
  match (f 1, g 2) with
  | (x, y) -> x + y
