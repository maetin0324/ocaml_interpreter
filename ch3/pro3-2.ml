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

type value =
| IntVal  of int
| BoolVal of bool  



let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v)::tl -> if x=y then v else lookup x tl 

let print_env env = 
  let rec print_env_aux env =
    match env with
    | [] -> ()
    | (x,v)::tl -> print_string (x ^ " = "); 
                   (match v with
                    | IntVal(n) -> print_int n
                    | BoolVal(b) -> print_string (string_of_bool b));
                   print_string "; ";
                   print_env_aux tl
  in print_string "[";
     print_env_aux env;
     print_string "]\n"
let ext env x v = (x, v)::env

let rec eval3 e env =
  let binop f e1 e2 env =
    match (eval3 e1 env, eval3 e2 env) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer values expected"
  in
  match e with
  | IntLit(n)  -> IntVal(n)
  | Plus(e1,e2) -> binop (+) e1 e2 env
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Eq(e1,e2) ->
      begin
        match (eval3 e1 env, eval3 e2 env) with
          | (IntVal(n1),IntVal(n2)) -> BoolVal(n1=n2)
          | (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1=b2)
          | _ -> failwith "wrong value"
      end
  | Greater(e1,e2) ->
      begin
        match (eval3 e1 env, eval3 e2 env) with
          | (IntVal(n1),IntVal(n2)) -> BoolVal(n1>n2)
          | _ -> failwith "wrong value"
      end
  | BoolLit(b) -> BoolVal(b)
  | If(e1,e2,e3) ->
      begin
        match (eval3 e1 env) with
          | BoolVal(true) -> eval3 e2 env
          | BoolVal(false) -> eval3 e3 env
          | _ -> failwith "wrong value"
      end
  | Var(x) -> lookup x env
  | Let(x,e1,e2) ->
    let env1 = ext env x (eval3 e1 env) 
    in let _ = print_env env1
    in eval3 e2 env1
  | _ -> failwith "unknown expression"

let exp1 = Let("x", IntLit 5, (Plus(Var("x"), IntLit 1)));;
let ans1 = eval3 exp1 [];;
let exp2 = Let("x", IntLit 5, (If((Greater(Var("x"), IntLit 1)), BoolLit true, BoolLit false)));;
let ans2 = eval3 exp2 [];;
let exp3 = Let("x", IntLit 5, (Let("x", IntLit 1, (Plus(Var("x"), IntLit 2)))));;
let ans3 = eval3 exp3 [];;
let exp4 = Let("x", IntLit 5, (Let("y", Plus(Var("x"), IntLit 1), (Plus(Var("x"), Var("y"))))));;
let ans4 = eval3 exp4 [];;
let exp5 = Let("x", IntLit 1, (Plus(Let("x", IntLit 2, (Plus(Var("x"), IntLit 1))), Times(Var("x"), IntLit 2))));;
let ans5 = eval3 exp5 [];;
