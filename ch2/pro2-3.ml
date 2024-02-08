type exp =
  IntLit of int
| Plus of exp * exp
| Times of exp * exp
| Minus of exp * exp
| Div of exp * exp

let rec eval1 e =
  match e with
  | IntLit(n) -> n
  | Plus(e1,e2) -> (eval1 e1) + (eval1 e2)
  | Times(e1,e2) -> (eval1 e1) * (eval1 e2)
  | Minus(e1,e2) -> (eval1 e1) - (eval1 e2)
  | Div(e1,e2) -> if (eval1 e2) == 0 then failwith "division by zero" else (eval1 e1) / (eval1 e2)
  | _ -> failwith "unknown expression"

let x1 = Plus(IntLit(1),IntLit(2))
let x2 = Times(IntLit(3),IntLit(4))
let x3 = Plus(IntLit(1), Times(IntLit(2),IntLit(3)))
let x4 = Div(IntLit(4),IntLit(0))
let x5 = Div(IntLit(4),IntLit(2))
let x6 = Div(IntLit(6),Minus(IntLit(4),IntLit(2)))
