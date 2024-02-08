type exp =
|  IntLit of int
|  Plus of exp * exp 
|  Times of exp * exp
|  BoolLit of bool
|  If of exp * exp * exp  
|  Eq of exp * exp
|  Greater of exp * exp  
|  Var of string        
|  Let of string * exp * exp 
|  Fun of string * exp
|  App of exp * exp

type value =
| IntVal  of int
| BoolVal of bool 
| FunVal of string * exp * ((string * value) list )



let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v)::tl -> if x=y then v else lookup x tl 

let ext env x v = (x, v)::env

let empty_env = []

let rec eval4 e env =
  let binop f e1 e2 env =
    match (eval4 e2 env, eval4 e1 env) with
    | (IntVal(n2),IntVal(n1)) -> IntVal(f n1 n2)
    | _ -> failwith "integer values expected"
  in
  match e with
  | IntLit(n)  -> IntVal(n)
  | Plus(e1,e2) -> binop (+) e1 e2 env
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Eq(e1,e2) ->
      begin
        match (eval4 e2 env, eval4 e1 env) with
          | (IntVal(n2),IntVal(n1)) -> BoolVal(n1=n2)
          | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1=b2)
          | _ -> failwith "wrong value"
      end
  | Greater(e1,e2) ->
      begin
        match (eval4 e2 env, eval4 e1 env) with
          | (IntVal(n2),IntVal(n1)) -> BoolVal(n1>n2)
          | _ -> failwith "wrong value"
      end
  | BoolLit(b) -> BoolVal(b)
  | If(e1,e2,e3) ->
      begin
        match (eval4 e1 env) with
          | BoolVal(true) -> eval4 e2 env
          | BoolVal(false) -> eval4 e3 env
          | _ -> failwith "wrong value"
      end
  | Var(x) -> lookup x env
  | Let(x,e1,e2) ->
    let env1 = ext env x (eval4 e1 env) 
    in eval4 e2 env1
  | Fun(x,e1) -> FunVal(x,e1,env)
  | App(e1,e2) -> 
    begin
      match eval4 e1 env with
        | FunVal(x,body,env1) -> 
          let arg = (eval4 e2 env)
          in eval4 body (ext env1 x arg)
        | _ -> failwith "function value expected"
    end
  | _ -> failwith "unknown expression"

let exp1 = Let("x", IntLit(1), Let("f", Fun("y", Plus(Var("x"), Var("y"))), Let("x", IntLit(2), App(Var("f"), Plus(Var("x"), IntLit(3))))))
let ans1 = eval4 exp1 empty_env