type exp =
  IntLit of int
| Plus of exp * exp
| Times of exp * exp
| Minus of exp * exp
| Div of exp * exp

let plustwo e = Plus (e, IntLit 2)

let rec abs e =
  match e with
    IntLit n -> if n < 0 then IntLit (-n) else e
    | Plus (e1, e2) -> Plus (abs e1, abs e2)
    | Times (e1, e2) -> Times (abs e1, abs e2)
    | Minus (e1, e2) -> Minus (abs e1, abs e2)
    | Div (e1, e2) -> Div (abs e1, abs e2)
