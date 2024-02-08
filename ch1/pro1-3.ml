(* Great Common Divisor  use Extended-Euclid algorithm*)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

(*Fibonacci number*)
let fib n =
  let rec fib_iter a b n =
    if n = 0 then a
    else fib_iter b (a+b) (n-1)
  in fib_iter 0 1 n

(*Returns the Nth prime number.*)
let rec prime n =
  let rec is_prime n i =
    if i * i > n then true
    else if n mod i = 0 then false
    else is_prime n (i+1)
  in
  let rec prime_iter n i =
    if n = 0 then i-1
    else if is_prime i 2 then prime_iter (n-1) (i+1)
    else prime_iter n (i+1)
  in prime_iter n 2;;

(*Test*)
print_int (gcd 13 27); print_newline();
print_int (gcd 12 (-2)); print_newline();
print_int (fib 7); print_newline();
print_int (fib 10); print_newline();
print_int (prime 1); print_newline();
print_int (prime 3); print_newline();
print_int (prime 5); print_newline();
