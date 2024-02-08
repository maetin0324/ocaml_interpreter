(* main.ml *)

(* 構文定義ファイル syntax.ml で定義された exp型を使う *)
open Syntax ;;

(* 与えられた文字列の字句解析と構文解析だけを行う関数 *)
(* parse : string -> exp *)

type result_tcheck = Ok of ty | Error of unit
type result_runi = Ok of value | Error of unit
type result_runc = Ok of cam_value | Error of unit

let parse str = 
  Parser.main Lexer.token 
    (Lexing.from_string str)

let tcheck str te : result_tcheck =
  try let e = parse str in
    try Ok(Eval.tcheck te e) with
      Failure s -> Error(print_string ("type error: " ^ s ^ "\n"))
  with
    Failure s -> Error(print_string ("parse error: " ^ s ^ "\n"))

let runi str : result_runi =
  try let e = parse str in
    try Ok(Eval.runi e) with
      Failure s -> Error(print_string ("runtime error: " ^ s ^ "\n"))
  with
    Failure s -> Error(print_string ("parse error: " ^ s ^ "\n"))

let runc str : result_runc =
  try let e = parse str in
    try Ok(Eval.runc e) with
      Failure s -> Error(print_string ("runtime error: " ^ s ^ "\n"))
  with
    Failure s -> Error(print_string ("parse error: " ^ s ^ "\n"))

(* メイン関数 *)

(* 時間測定用の関数 *)
let time : (unit -> 'a) -> 'a * float =
  fun f ->
    let start = Sys.time () in
    let res   = f () in
    let end_  = Sys.time () in
    (res, end_ -. start)
