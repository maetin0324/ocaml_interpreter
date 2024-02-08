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

type tyvar = string
type ty = TInt | TBool | TArrow of ty * ty | TVar of tyvar
type tyenv = (string * ty) list
type tysubst = (tyvar * ty) list

type result_ty = Ok of ty | Error of string

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x=y then v else lookup x tl 

let ext env x v = (x, v)::env

let string_of_exp e =
  let rec str e =
    match e with
    | IntLit(i) -> string_of_int i
    | Plus(e1,e2) -> "(" ^ str e1 ^ " + " ^ str e2 ^ ")"
    | Times(e1,e2) -> "(" ^ str e1 ^ " * " ^ str e2 ^ ")"
    | BoolLit(true) -> "true"
    | BoolLit(false) -> "false"
    | And(e1,e2) -> "(" ^ str e1 ^ " && " ^ str e2 ^ ")"
    | Or(e1,e2) -> "(" ^ str e1 ^ " || " ^ str e2 ^ ")"
    | If(e1,e2,e3) -> "if " ^ str e1 ^ " then " ^ str e2 ^ " else " ^ str e3
    | Eq(e1,e2) -> "(" ^ str e1 ^ " = " ^ str e2 ^ ")"
    | Greater(e1,e2) -> "(" ^ str e1 ^ " > " ^ str e2 ^ ")"
    | Var(x) -> x
    | Let(x,e1,e2) -> "let " ^ x ^ " = " ^ str e1 ^ " in " ^ str e2
    | LetRec(f,x,e1,e2) -> "let rec " ^ f ^ " " ^ x ^ " = " ^ str e1 ^ " in " ^ str e2
    | Fun(x,e) -> "(fun " ^ x ^ " -> " ^ str e ^ ")"
    | App(e1,e2) -> "(" ^ str e1 ^ " " ^ str e2 ^ ")"
  in str e

let string_of_ty t=
  let rec str t =
    match t with
    | TInt -> "int"
    | TBool -> "bool"
    | TArrow(t1,t2) -> "(" ^ str t1 ^ " -> " ^ str t2 ^ ")"
  in str t

let string_of_tenv tenv =
  let rec str tenv =
    match tenv with
    | [] -> "\n"
    | (x,t)::tl -> x ^ ":" ^ string_of_ty t ^ ", " ^ str tl
  in str tenv




(* 型検査器 *)

let rec tcheck3 te e =
  match e with
  | Var(s)       -> lookup s te
  | IntLit(_)    -> TInt
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Plus"
      end
  | Times(e1,e2) ->
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Times"
      end
  | If(e1,e2,e3) -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2, tcheck3 te e3) with
          (TBool,t1,t2) -> if t1=t2 then t1
                           else failwith "type error in IF"
        | _ -> failwith "type error in IF"
      end
  | Eq(e1,e2) -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
          (TInt,TInt) -> TBool
        | (TBool,TBool) -> TBool
        | _ -> failwith "type error in Eq"
      end
  | Fun(x, e1) ->
    let t1 = lookup x te in
    let t2 = tcheck3 te e1 in 
      TArrow(t1,t2)
  | App(e1,e2) ->
    let t1 = tcheck3 te e1 in
    let t2 = tcheck3 te e2 in 
    begin
      match t1 with
      | TArrow(t10,t11) -> if t2=t10 then t11
                              else failwith "type error in App1"
      | _ -> failwith "type error in App2"
    end
  | _ -> failwith "unknown expression"


let test_tcheck3 t e : result_ty =
  try Ok(tcheck3 t e)
  with Failure s -> Error s

let print_test_tcheck3 t e =
  match test_tcheck3 t e with
  | Ok(ty) -> "type of " ^ string_of_exp e ^ "in\n" ^ string_of_tenv t ^ "is " ^ string_of_ty ty ^ "\n"
  | Error(s) -> "type error in " ^ string_of_exp e ^ ": " ^ s ^ "\n"

let e1 = Fun("x", If(BoolLit(true), Var("x"), IntLit(100)))
let t1 = [("x", TInt)]
let e2 = Fun("x", If(BoolLit(true), Var("x"), IntLit(100)))
let t2 = [("x", TBool)]
let e3 = App(Fun("x", If(BoolLit true, Var "x", IntLit 100)), If(BoolLit true, Var "y", IntLit 200))
let t3 = [("x",TInt);("y",TInt)]
let e4 = Fun("f", Fun("x", App(Var "f", App(Var "f", Plus(App(Var "f", Var "x"), IntLit 10)))))
let t4 = [("f", TArrow(TInt, TInt)); ("x", TInt)]
let e5 = Fun("f", Fun("g", Fun("x", App(Var "f", App(Var "g", Var "x")))))
let t5 = [("f", TArrow(TInt, TInt)); ("g", TArrow(TInt, TInt)); ("x", TInt)]
let e6 = If(BoolLit true, Fun("x", Plus(Var "x", IntLit 1)), Fun("y", Times (Var "y", IntLit 2)))
let t6 = [("x",TInt); ("y",TInt)]

let _ = print_string (print_test_tcheck3 t1 e1)
let _ = print_string (print_test_tcheck3 t2 e2)
let _ = print_string (print_test_tcheck3 t3 e3)
let _ = print_string (print_test_tcheck3 t4 e4)
let _ = print_string (print_test_tcheck3 t5 e5)
let _ = print_string (print_test_tcheck3 t6 e6)
