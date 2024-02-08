open Syntax

(* インタープリタ *)

let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v)::tl -> if x=y then v else lookup x tl 

let ext env x v = (x, v)::env

let empty_env = []

let rec eval e env =
  let binop f e1 e2 env =
    match (eval e2 env, eval e1 env) with
    | (IntVal(n2),IntVal(n1)) -> IntVal(f n1 n2)
    | _ -> failwith "integer values expected in binop"
  in
  match e with
  | IntLit(n)  -> IntVal(n)
  | Plus(e1,e2) -> binop (+) e1 e2 env
  | Minus(e1,e2) -> binop (-) e1 e2 env
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Div(e1,e2) -> binop (/) e1 e2 env
  | Eq(e1,e2) ->
      begin
        match (eval e2 env, eval e1 env) with
          | (IntVal(n2),IntVal(n1)) -> BoolVal(n1=n2)
          | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1=b2)
          | _ -> failwith "wrong value in Eq"
      end
  | Greater(e1,e2) ->
      begin
        match (eval e2 env, eval e1 env) with
          | (IntVal(n2),IntVal(n1)) -> BoolVal(n1>n2)
          | _ -> failwith "wrong value in Greater"
      end
  | Less(e1,e2) ->
      begin
        match (eval e2 env, eval e1 env) with
          | (IntVal(n2),IntVal(n1)) -> BoolVal(n1<n2)
          | _ -> failwith "wrong value in Less"
      end
  | BoolLit(b) -> BoolVal(b)
  | And(e1,e2) ->
      begin
        match (eval e2 env, eval e1 env) with
          | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1 && b2)
          | _ -> failwith "wrong value in And"
      end
  | Or(e1,e2) ->
      begin
        match (eval e2 env, eval e1 env) with
          | (BoolVal(b2),BoolVal(b1)) -> BoolVal(b1 || b2)
          | _ -> failwith "wrong value in Or"
      end
  | If(e1,e2,e3) ->
      begin
        match (eval e1 env) with
          | BoolVal(true) -> eval e2 env
          | BoolVal(false) -> eval e3 env
          | _ -> failwith "wrong value in If"
      end
  
  | Var(x) -> lookup x env
  | Let(x,e1,e2) ->
    let env1 = ext env x (eval e1 env) 
    in eval e2 env1
  | LetRec(f,x,e1,e2) ->
    let env1 = ext env f (RecFunVal (f, x, e1, env))
    in eval e2 env1
  | Fun(x,e1) -> FunVal(x,e1,env)
  | App(e1,e2) ->
    let funpart = (eval e1 env) in
    let arg = (eval e2 env) in
      begin
        match funpart with
          | FunVal(x,body,env1) ->
            let env2 = (ext env1 x arg) in
            eval body env2
          | RecFunVal(f,x,body,env1) ->
            let env2 = (ext (ext env1 x arg) f funpart) in
            eval body env2
          | _ -> failwith "wrong value in App"
      end
  | _ -> failwith "unknown expression"

let runi e = eval e empty_env

(* 型検査*)

let rec tcheck te e =
  match e with
  | Var(s)       -> lookup s te
  | IntLit(_)    -> TInt
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
      begin
        match (tcheck te e1, tcheck te e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Plus"
      end
  | Times(e1,e2) ->
      begin
        match (tcheck te e1, tcheck te e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Times"
      end
  | If(e1,e2,e3) -> 
      begin
        match (tcheck te e1, tcheck te e2, tcheck te e3) with
          (TBool,t1,t2) -> if t1=t2 then t1
                           else failwith "type error in IF"
        | _ -> failwith "type error in IF"
      end
  | Eq(e1,e2) -> 
      begin
        match (tcheck te e1, tcheck te e2) with
          (TInt,TInt) -> TBool
        | (TBool,TBool) -> TBool
        | _ -> failwith "type error in Eq"
      end
  | Fun(x, e1) ->
    let t1 = lookup x te in
    let t2 = tcheck te e1 in 
      TArrow(t1,t2)
  | App(e1,e2) ->
    let t1 = tcheck te e1 in
    let t2 = tcheck te e2 in 
    begin
      match t1 with
      | TArrow(t10,t11) -> if t2=t10 then t11
                              else failwith "type error in App1"
      | _ -> failwith "type error in App2"
    end
  | _ -> failwith "unknown expression"

(* CAM *)

let rec position (x : string) (venv : string list) : int =
  match venv with
    | [] -> failwith "no matching variable in environment"
    | y::venv2 -> if x=y then 0 else (position x venv2) + 1

let rec c e venv =
  match e with
    | IntLit n -> [CAM_Ldi n]
    | Plus(e1,e2) -> (c e2 venv) @ (c e1 venv) @ [CAM_Add]
    | Minus(e1,e2) -> (c e2 venv) @ (c e1 venv) @ [CAM_Minus]
    | Times(e1,e2) -> (c e2 venv) @ (c e1 venv) @ [CAM_Mult]
    | BoolLit b -> [CAM_Ldb b]
    | If(e1,e2,e3) -> (c e1 venv) @ [CAM_Test(c e2 venv, c e3 venv)]
    | Eq(e1,e2) -> (c e2 venv) @ (c e1 venv) @ [CAM_Eq]
    | And(e1,e2) -> (c e2 venv) @ (c e1 venv) @ [CAM_And]
    | Or(e1,e2) -> (c e2 venv) @ (c e1 venv) @ [CAM_Or]
    | Var x -> [CAM_Access (position x venv)]
    | Let(x,e1,e2) -> (c e1 venv) @ [CAM_Let] @ (c e2 (x::venv)) @ [CAM_EndLet]
    | LetRec(f,x,e1,e2) -> [CAM_Closure((c e1 (x::f::venv)) @ [CAM_Return])] @ [CAM_Let] @ (c e2 (f::venv)) @ [CAM_EndLet]
    | Fun(x,e) -> [CAM_Closure((c e (x::venv)) @ [CAM_Return])]
    | App(e1,e2) -> (c e2 venv) @ (c e1 venv) @ [CAM_Apply]

let compile (e : exp) : cam_code =
  c e []

(* CAM のスタックマシンのコードを実行する関数 *)
let rec exec code cam_env cam_stack =
  match code with
    [] -> List.hd cam_stack
    | CAM_Ldi(n)::rest -> exec rest cam_env (CAM_IntVal(n)::cam_stack)
    | CAM_Ldb(b)::rest -> exec rest cam_env (CAM_BoolVal(b)::cam_stack)
    | CAM_Access(i)::rest -> exec rest cam_env ((List.nth cam_env i)::cam_stack)
    | CAM_Closure(c)::rest -> exec rest cam_env (CAM_ClosVal(c,cam_env)::cam_stack)
    | CAM_Apply::rest ->
      begin
        match cam_stack with
        | CAM_ClosVal(c,cam_env')::v::s' -> exec c (v::CAM_ClosVal(c, cam_env')::cam_env') (CAM_ClosVal(rest,cam_env)::s') 
        | _ -> failwith "Error in CAM_Apply"
      end
    | CAM_Return::rest ->
      begin
        match cam_stack with
        | v::CAM_ClosVal(c,cam_env')::s' -> exec c cam_env' (v::s')
        | _ -> failwith "Error in CAM_Return"
      end
    | CAM_Let::rest ->
      begin
        match cam_stack with
        | v::s' -> exec rest (v::cam_env) s'
        | _ -> failwith "Error in CAM_Let"
      end
    | CAM_EndLet::rest ->
      begin
        match cam_env with
          | v::cam_env' -> exec rest cam_env' cam_stack
          | _ -> failwith "Error in CAM_Endlet"
      end
    | CAM_Test(c1,c2)::rest ->
      begin
        match cam_stack with
        | CAM_BoolVal(true)::s' -> exec (c1@rest) cam_env s'
        | CAM_BoolVal(false)::s' -> exec (c2@rest) cam_env s'
      end
    | CAM_Add::rest ->
      begin
        match cam_stack with
        | CAM_IntVal(n2)::CAM_IntVal(n1)::s' -> exec rest cam_env (CAM_IntVal(n1+n2)::s')
        | _ -> failwith "Error in CAM_Add"
      end
    | CAM_Minus::rest ->
      begin
        match cam_stack with
        | CAM_IntVal(n2)::CAM_IntVal(n1)::s' -> exec rest cam_env (CAM_IntVal(n2-n1)::s')
        | _ -> failwith "Error in CAM_Minus"
      end
    | CAM_Mult::rest ->
      begin
        match cam_stack with
        | CAM_IntVal(n2)::CAM_IntVal(n1)::s' -> exec rest cam_env (CAM_IntVal(n1*n2)::s')
        | _ -> failwith "Error in CAM_Mult"
      end
    | CAM_Eq::rest ->
      begin
        match cam_stack with
        | CAM_IntVal(n2)::CAM_IntVal(n1)::s' -> exec rest cam_env (CAM_BoolVal(n1=n2)::s')
        | _ -> failwith "Error in CAM_Eq"
      end
    | CAM_And::rest ->
      begin
        match cam_stack with
        | CAM_BoolVal(b2)::CAM_BoolVal(b1)::s' -> exec rest cam_env (CAM_BoolVal(b1 && b2)::s')
        | _ -> failwith "Error in CAM_And"
      end
    | CAM_Or::rest ->
      begin
        match cam_stack with
        | CAM_BoolVal(b2)::CAM_BoolVal(b1)::s' -> exec rest cam_env (CAM_BoolVal(b1 || b2)::s')
        | _ -> failwith "Error in CAM_Or"
      end
    | _ -> failwith "Error in exec"

let runc e = exec (compile e) [] []
