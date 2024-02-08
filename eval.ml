open Syntax

let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v)::tl -> if x=y then v else lookup x tl 

let ext env x v = (x, v)::env

let empty_env = []

let rec eval e env =
  let binop f e1 e2 env =
    match (eval e1 env, eval e2 env) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer values expected"
  in
  match e with
  | IntLit(n)  -> IntVal(n)
  | Plus(e1,e2) -> binop (+) e1 e2 env
  | Minus(e1,e2) -> binop (-) e1 e2 env 
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Div(e1,e2) -> binop (/) e1 e2 env
  | Eq(e1,e2) ->
      begin
        match (eval e1 env, eval e2 env) with
          | (IntVal(n1),IntVal(n2)) -> BoolVal(n1=n2)
          | (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1=b2)
          | _ -> failwith "wrong value"
      end
  | NotEq(e1,e2) ->
      begin
        match (eval e1 env, eval e2 env) with
          | (IntVal(n1),IntVal(n2)) -> BoolVal(n1!=n2)
          | (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1!=b2)
          | _ -> failwith "wrong value"
      end
  | Greater(e1,e2) ->
      begin
        match (eval e1 env, eval e2 env) with
          | (IntVal(n1),IntVal(n2)) -> BoolVal(n1>n2)
          | _ -> failwith "wrong value"
      end
  | Less(e1,e2) ->
      begin
        match (eval e1 env, eval e2 env) with
          | (IntVal(n1),IntVal(n2)) -> BoolVal(n1<n2)
          | _ -> failwith "wrong value"
      end
  | BoolLit(b) -> BoolVal(b)
  | If(e1,e2,e3) ->
      begin
        match (eval e1 env) with
          | BoolVal(true) -> eval e2 env
          | BoolVal(false) -> eval e3 env
          | _ -> failwith "wrong value"
      end
  | Var(x) -> lookup x env
  | Let(x,e1,e2) ->
    let env1 = ext env x (eval e1 env) 
    in eval e2 env1
  | Fun(x,e1) -> FunVal(x,e1,env)
  | App(e1,e2) -> 
    begin
      match eval e1 env with
        | FunVal(x,body,env1) -> 
          let arg = (eval e2 env)
          in eval body (ext env1 x arg)
        | _ -> failwith "function value expected"
    end
  | _ -> failwith "unknown expression"