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

type ty = TInt | TBool

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x=y then v else lookup x tl 

let rec tcheck2 te e =
  match e with
  | Var(s)       -> lookup s te
  | IntLit(_)    -> TInt
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Plus"
      end
  | If(e1,e2,e3) -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2, tcheck2 te e3) with
          (TBool,TInt,TInt) -> TInt
        | (TBool,TBool,TBool) -> TBool
        | _ -> failwith "type error in IF"
      end
  | Eq(e1,e2) -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2) with
          (TInt,TInt) -> TBool
        | (TBool,TBool) -> TBool
        | _ -> failwith "type error in Eq"
      end
  | _ -> failwith "unknown expression"

let e1 = If(BoolLit(true), Var("x"), IntLit(100))
let e2 = If(Var("x"), Var("x"), IntLit(100))

let exp1 = [("x",TInt);("y",TInt)]
let exp2 = [("x",TBool);("y",TInt)]
let exp3 = [("z",TInt);("y",TInt)]
let exp4 = [("x",TInt)]
let exp5 = [("x",TBool)]
