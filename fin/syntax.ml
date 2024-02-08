(* syntax.ml *)

(* 式の型 *)
type exp = 
  | Var of string         (* variable e.g. x *)
  | IntLit of int         (* integer literal e.g. 17 *)
  | BoolLit of bool       (* boolean literal e.g. true *)
  | And of exp * exp      (* e && e *)
  | Or of exp * exp       (* e || e *)
  | If of exp * exp * exp (* if e then e else e *)
  | Let of string * exp * exp   (* let x=e in e *)
  | LetRec of string * string * exp * exp   (* letrec f x=e in e *)
  | Fun of string * exp   (* fun x -> e *)
  | App of exp * exp      (* function application i.e. e e *)
  | Eq of exp * exp       (* e = e *)
  | NotEq of exp * exp    (* e <> e *)
  | Greater of exp * exp  (* e > e *)
  | Less of exp * exp     (* e < e *)
  | Plus of exp * exp     (* e + e *)
  | Minus of exp * exp    (* e - e *)
  | Times of exp * exp    (* e * e *)
  | Div of exp * exp      (* e / e *)
  | Empty                 (* [ ] *)
  | Match of exp * ((exp * exp) list)    (* match e with e->e | ... *)
  | Cons of exp * exp     (* e :: e *)
  | Head of exp           (* List.hd e *)
  | Tail of exp           (* List.tl e *)

(* 値の型 *)
type value = 
  | IntVal  of int        (* integer value e.g. 17 *)
  | BoolVal of bool       (* booleanvalue e.g. true *)
  | ListVal of value list (* list value e.g. [1;2;3] *)
  | FunVal  of string * exp * env
                          (* function value e.g. \x. x+1 with env *)
  | RecFunVal of string * string * exp * env
                          (* recursive function value: solution-1 *)
                          (* let rec f x = e1 in e2 *)
  | RecFunVal2 of string * string * exp * env ref
                          (* recursive function value: solution-2 *)
                          (* let rec f x = e1 in e2 *)
and
  env = (string * value) list

(* 型検査器の型 *)
type ty = TInt | TBool | TArrow of ty * ty 


(* CAMマシンの型 *)

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
  | CAM_And                           (* スタックトップの値とスタックの上から2番めの値を
                     * 取り出し、それらが両方とも true であるかどうかテストし、
                     * その結果の真理値をスタックに積む
                     *)
  | CAM_Or                            (* スタックトップの値とスタックの上から2番めの値を
                      * 取り出し、それらのどちらかが true であるかどうかテストし、
                      * その結果の真理値をスタックに積む
                      *)
and cam_code = cam_instr list  (* コードは、命令の列である *)

type cam_value =  
  | CAM_IntVal  of int   (* CAM の値に対するタグにはCAM_ をつける *)
  | CAM_BoolVal of bool
  | CAM_ClosVal of cam_code * cam_env  (* 再帰関数に対応するクロージャ *)
and cam_stack = cam_value list (* スタック *)
and cam_env = cam_value list (* 環境は、1つのスタックフレームに相当する。 *)