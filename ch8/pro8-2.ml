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

(* CAM のスタックマシンのコードを出力する関数 *)
let rec print_cam_code = function
    [] -> ()
  | CAM_Ldi(n)::rest ->
      print_string "Ldi("; print_int n; print_string ")"; print_newline();
      print_cam_code rest
  | CAM_Ldb(b)::rest ->
      print_string "Ldb("; print_string (string_of_bool b); print_string ")"; print_newline();
      print_cam_code rest
  | CAM_Access(i)::rest ->
      print_string "Access("; print_int i; print_string ")"; print_newline();
      print_cam_code rest
  | CAM_Closure(c)::rest ->
      print_string "Closure(";
      print_cam_code c;
      print_string ")"; print_newline();
      print_cam_code rest
  | CAM_Apply::rest ->
      print_string "Apply"; print_newline();
      print_cam_code rest
  | CAM_Return::rest ->
      print_string "Return"; print_newline();
      print_cam_code rest
  | CAM_Let::rest ->
      print_string "Let"; print_newline();
      print_cam_code rest
  | CAM_EndLet::rest ->
      print_string "EndLet"; print_newline();
      print_cam_code rest
  | CAM_Test(c1,c2)::rest ->
      print_string "Test(";
      print_cam_code c1;
      print_string ",";
      print_cam_code c2;
      print_string ")"; print_newline();
      print_cam_code rest
  | CAM_Add::rest ->
      print_string "Add"; print_newline();
      print_cam_code rest
  | CAM_Eq::rest ->
      print_string "Eq"; print_newline();
      print_cam_code rest

let print_cam_instr instr =
  print_cam_code [instr]

let rec print_cam_env cam_env =
  match cam_env with
    [] -> ()
  | CAM_IntVal(n)::rest ->
      print_string "IntVal("; print_int n; print_string ")"; print_newline();
      print_cam_env rest
  | CAM_BoolVal(b)::rest ->
      print_string "BoolVal("; print_string (string_of_bool b); print_string ")"; print_newline();
      print_cam_env rest
  | CAM_ClosVal(c,cam_env')::rest ->
      print_string "ClosVal(";
      print_cam_code c;
      print_string ",";
      print_cam_env cam_env';
      print_string ")"; print_newline();
      print_cam_env rest

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


let c1 = [CAM_Ldi(4); CAM_Ldi(3); CAM_Ldi(2); CAM_Ldi(1); CAM_Add; CAM_Add; CAM_Add]
let ans1 = exec c1 [] []
let c2 = [CAM_Closure
  [CAM_Ldi 1; CAM_Access 0; CAM_Eq;
  CAM_Test ([CAM_Ldi 1],
            [CAM_Ldi (-1); CAM_Access 0; CAM_Add; CAM_Access 1;
              CAM_Apply; CAM_Access 0; CAM_Add]);
  CAM_Return];
CAM_Let; CAM_Ldi 10; CAM_Access 0; CAM_Apply; CAM_EndLet]
let ans2 = exec c2 [] []
let c3 = [CAM_Ldi(5); CAM_Ldi(3); CAM_Minus]
let ans3 = exec c3 [] []
let c4 = [CAM_Ldi(5); CAM_Ldi(3); CAM_Mult]
let ans4 = exec c4 [] []
