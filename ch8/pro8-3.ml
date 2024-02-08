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
|  LetRec of string * string * exp * exp
|  Fun of string * exp
|  App of exp * exp

type value =
| IntVal  of int
| BoolVal of bool 
| FunVal of string * exp * ((string * value) list )
| RecFunVal of string * string * exp * ((string * value) list )

type cam_instr =    
  | CAM_Ldi of int                    (* CAM_Ldi(n) は、整数 n をスタックに積む (loadする) *)
  | CAM_Ldb of bool                   (* CAM_Ldb(b) は、真理値 b をスタックに積む (loadする) *)
  | CAM_Access of int                 (* CAM_Access(i) は、環境の i+1 番目の値をスタックに積む *)
  | CAM_Closure of cam_code           (* CAM_Closure(c) は、関数本体のコードが c で、
			               * その環境が、現在の環境であるような関数
			               * クロージャを生成し、それをスタックに積む。
			               * 前項で説明したように変数は名前の代わりに
			               * 環境のインデックスで参照されるので、
			               * このクロージャにも関数引数は含まれない。
			               * なお、この関数クロージャは、再帰関数で
			               * あるとして処理される。
			             *)
  | CAM_Apply                         (* スタックトップの値が関数クロージャならば、
			               * その関数を、スタックの上から2番めにある値に
			               * 関数適用した計算を行なう。
			               *)
  | CAM_Return                        (* 関数の呼び出し元に戻る *)
  | CAM_Let                           (* スタックトップの値を環境の先頭に移す (環境を拡張する) *)
  | CAM_EndLet                        (* 環境の先頭の値を取り除く *)
  | CAM_Test of cam_code * cam_code   (* I_Test(c1,c2)は、スタックトップの値が
			               * true ならば、コードc1 を実行し、false
			               * ならばコード c2 を実行する。
			               *)
  | CAM_Add                           (* スタックトップの値とスタックの上から2番めの値を
			               * 取り出し、その和をスタックに積む
			               *)
  | CAM_Minus                         (* スタックトップの値とスタックの上から2番めの値を
                     * 取り出し、その差をスタックに積む
                     *)
  | CAM_Mult                          (* スタックトップの値とスタックの上から2番めの値を
                     * 取り出し、その積をスタックに積む
                     *)
  | CAM_Eq                            (* スタックトップの値とスタックの上から2番めの値を
			               * 取り出し、それらが同じ整数であるかどうかテストし、
			               * その結果の真理値をスタックに積む
			               *)
and cam_code = cam_instr list  (* コードは、命令の列である *)

type cam_value =  
  | CAM_IntVal  of int   (* CAM の値に対するタグにはCAM_ をつける *)
  | CAM_BoolVal of bool
  | CAM_ClosVal of cam_code * cam_env  (* 再帰関数に対応するクロージャ *)
and cam_stack = cam_value list (* スタック *)
and cam_env = cam_value list (* 環境は、1つのスタックフレームに相当する。 *)

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
| BoolLit(b) -> BoolVal(b)
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

let rec position (x : string) (venv : string list) : int =
  match venv with
    | [] -> failwith "no matching variable in environment"
    | y::venv2 -> if x=y then 0 else (position x venv2) + 1

(* ocamlのexpをCAMの実行コードにコンパイルする *)

let rec c e venv =
  match e with
    | IntLit n -> [CAM_Ldi n]
    | Plus(e1,e2) -> (c e2 venv) @ (c e1 venv) @ [CAM_Add]
    | Times(e1,e2) -> (c e2 venv) @ (c e1 venv) @ [CAM_Mult]
    | BoolLit b -> [CAM_Ldb b]
    | If(e1,e2,e3) -> (c e1 venv) @ [CAM_Test(c e2 venv, c e3 venv)]
    | Eq(e1,e2) -> (c e2 venv) @ (c e1 venv) @ [CAM_Eq]
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
        | CAM_IntVal(n2)::CAM_IntVal(n1)::s' -> exec rest cam_env (CAM_IntVal(n1-n2)::s')
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
    | _ -> failwith "Error in exec"

(* CAMの実行コードを実行する *)
let e1 = Let("x", IntLit 1, Let("y", IntLit 2, Plus(Var "x", Var "y")))
let cans1 = exec (compile e1) [] []
let ians1 = eval6 e1 empty_env
let e2 = Let("x", IntLit 1, Let("y", IntLit 2, If(Eq(Var "x", Var "y"), IntLit 3, IntLit 4)))
let cans2 = exec (compile e2) [] []
let ians2 = eval6 e2 empty_env
let e3 = Let("f", Fun("x", Plus(Var "x", IntLit 1)), App(Var "f", IntLit 2))
let cans3 = exec (compile e3) [] []
let ians3 = eval6 e3 empty_env
let e4 = LetRec("f", "x", If(Eq(Var("x"), IntLit(0)), IntLit(1), Times(Var("x"), App(Var("f"), Plus(Var("x"), IntLit(-1))))), App(Var("f"), IntLit(5)))
let cans4 = exec (compile e4) [] []
let ians4 = eval6 e4 empty_env
