open Syntax

let run_bad1 = "1 + + 1"
let run_bad2 = "let let"
let run_bad3 = "1 = true"
let run_ok1 = "1 + 1"
let run_ok2 = "let x = 1 in let y = 2 in x + y"
let run_ok3 = "let rec fib n = if (n = 0) || (n = 1) then 1 else fib (n - 1) + fib (n - 2) in fib 10"

let tcheck_bad1 = "1 + true"
let tcheck_bad2 = "let x = 1 in x + true"
let tcheck_ok = "(fun f -> (fun g -> (fun x -> f (g x))))"
let te_ok = [("f", TArrow(TInt, TInt)); ("g", TArrow(TInt, TInt)); ("x", TInt)]