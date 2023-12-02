putStrLn ""
putStrLn "Some Classic Examples:"
putStrLn ""

putStrLn "Fibonacci:"
let fib n = case n of {
  0 -> 1,
  1 -> 1,
  _ -> fib (n-1) + fib (n-2)
} :: Nat -> Nat

putStrLn "fib 1 ="
print fib 1
putStrLn "fib 10 ="
print fib 10

putStrLn ""
putStrLn "Factorial"
let fac n = case n of {
  0 -> 1,
  _ -> n * fac (n-1)
} :: Nat -> Nat

putStrLn "fac 1 ="
print fac 1
putStrLn "fac 4 ="
print fac 4
