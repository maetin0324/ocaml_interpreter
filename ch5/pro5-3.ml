type exp =
|  IntLit of int
|  Plus of exp * exp 
|  Times of exp * exp
|  BoolLit of bool
|  And of exp * exp
|  Or of exp * exp
|  If of exp * exp * exp  
|  Eq of exp * exp
|  Greater of exp * exp  
|  Var of string        
|  Let of string * exp * exp 
|  LetRec of string * string * exp * exp
|  Fun of string * exp
|  App of exp * exp

type value =
| IntVal  of int
| BoolVal of bool 
| FunVal of string * exp * ((string * value) list )
| RecFunVal of string * string * exp * ((string * value) list )



let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v)::tl -> if x=y then v else lookup x tl 

let ext env x v = (x, v)::env

let empty_env = []

let rec eval6 e env =
  let binop f e1 e2 env =
    match (eval6 e2 env, eval6 e1 env) with
    | (IntVal(n2),IntVal(n1)) -> IntVal(f n1 n2)
    | _ -> failwith "integer values expected"
  in
  match e with
  | IntLit(n)  -> IntVal(n)
  | Plus(e1,e2) -> binop (+) e1 e2 env
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Eq(e1,e2) ->
      begin
        match (eval6 e2 env, eval6 e1 env) with
          | (IntVal(n2),IntVal(n1)) -> BoolVal(n1=n2)
          | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1=b2)
          | _ -> failwith "wrong value"
      end
  | Greater(e1,e2) ->
      begin
        match (eval6 e2 env, eval6 e1 env) with
          | (IntVal(n2),IntVal(n1)) -> BoolVal(n1>n2)
          | _ -> failwith "wrong value"
      end
  | BoolLit(b) -> BoolVal(b)
  | And(e1,e2) ->
      begin
        match (eval6 e2 env, eval6 e1 env) with
          | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1 && b2)
          | _ -> failwith "wrong value"
      end
  | Or(e1,e2) ->
      begin
        match (eval6 e2 env, eval6 e1 env) with
          | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1 || b2)
          | _ -> failwith "wrong value"
      end
  | If(e1,e2,e3) ->
      begin
        match (eval6 e1 env) with
          | BoolVal(true) -> eval6 e2 env
          | BoolVal(false) -> eval6 e3 env
          | _ -> failwith "wrong value"
      end
  
  | Var(x) -> lookup x env
  | Let(x,e1,e2) ->
    let env1 = ext env x (eval6 e1 env) 
    in eval6 e2 env1
  | LetRec(f,x,e1,e2) ->
    let env1 = ext env f (RecFunVal (f, x, e1, env))
    in eval6 e2 env1
  | Fun(x,e1) -> FunVal(x,e1,env)
  | App(e1,e2) ->
    let funpart = (eval6 e1 env) in
    let arg = (eval6 e2 env) in
      begin
        match funpart with
          | FunVal(x,body,env1) ->
            let env2 = (ext env1 x arg) in
            eval6 body env2
          | RecFunVal(f,x,body,env1) ->
            let env2 = (ext (ext env1 x arg) f funpart) in
            eval6 body env2
          | _ -> failwith "wrong value in App"
      end
  | _ -> failwith "unknown expression"

let exp1 = LetRec("f", "x", Var("x"), IntLit(0))
let ans1 = eval6 exp1 empty_env
let exp2 = LetRec("f", "x", Var("x"), App(Var("f"), IntLit(0)))
let ans2 = eval6 exp2 empty_env
let exp3 = LetRec("f", "x", If(Eq(Var("x"), IntLit(0)), IntLit(1), Plus(IntLit(2), App(Var("f"), Plus(Var("x"), IntLit(-1))))), App(Var("f"), IntLit(0)))
let ans3 = eval6 exp3 empty_env
let exp4 = LetRec("f", "x", If(Eq(Var("x"), IntLit(0)), IntLit(1), Times(Var("x"), App(Var("f"), Plus(Var("x"), IntLit(-1))))), App(Var("f"), IntLit(3)))
let ans4 = eval6 exp4 empty_env
let exp5 = LetRec("f", "x", If(Eq(Var("x"), IntLit(0)), IntLit(1), Times(Var("x"), App(Var("f"), Plus(Var("x"), IntLit(-1))))), App(Var("f"), IntLit(5)))
let ans5 = eval6 exp5 empty_env
let exp_fib = LetRec("fib", "x", If(Or(Eq(Var("x"), IntLit(0)), Eq(Var("x"), IntLit(1))), IntLit(1), Plus(App(Var("fib"), Plus(Var("x"), IntLit(-1))), App(Var("fib"), Plus(Var("x"), IntLit(-2))))), App(Var("fib"), IntLit(5)))
let ans_fib = eval6 exp_fib empty_env