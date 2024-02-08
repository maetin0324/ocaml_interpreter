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

let rec tcheck1 e =
  match e with
  | IntLit(_)    -> TInt
  | BoolLit(_)   -> TBool
  | Plus(e1,e2)  -> 
      begin
        match (tcheck1 e1, tcheck1 e2) with
          (TInt,TInt) -> TInt
        | _ -> failwith "type error in Plus"
      end
  | If(e1,e2,e3) -> 
      begin
        match (tcheck1 e1, tcheck1 e2, tcheck1 e3) with
          (TBool,TInt,TInt) -> TInt
        | (TBool,TBool,TBool) -> TBool
        | _ -> failwith "type error in IF"
      end
  | Eq(e1,e2) -> 
      begin
        match (tcheck1 e1, tcheck1 e2) with
          (TInt,TInt) -> TBool
        | (TBool,TBool) -> TBool
        | _ -> failwith "type error in Eq"
      end
  | _ -> failwith "unknown expression"

let exp1 = If(BoolLit(true), IntLit(1), IntLit(100))
let exp2 = If(BoolLit(true), Plus(IntLit(1), BoolLit(false)), IntLit(100))
let exp3 = Eq(Plus(IntLit(1), IntLit(2)), IntLit(5))
let exp4 = Eq(Plus(IntLit(1), IntLit(2)), BoolLit(true))
let ans1 = tcheck1 exp1
let ans2 = tcheck1 exp2
let ans3 = tcheck1 exp3
let ans4 = tcheck1 exp4
