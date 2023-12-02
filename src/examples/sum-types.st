putStrLn ""
putStrLn "Examples of Sum Types"
putStrLn ""

-- sum types
assume (Result :: * -> * -> *)
assume (Ok :: a -> Result a b)
assume (Err :: b -> Result a b)

assume (Option :: * -> *)
assume (Some :: a -> Option a)
assume (None :: Option a)

let isOk res = case res of {
  (Ok _)  -> True,
  (Err _) -> False
} :: Result a -> Bool

putStrLn "We can create sum types:"
print Ok "This is OK"
print Err "This is an Err"
print Some 1
print None
putStrLn ""
putStrLn "And we can pattern match on them:"
putStrLn "isOk (Ok 1) ="
print isOk (Ok 1)
putStrLn "isOk (Err \"oh no\") ="
print isOk (Err "oh no")
putStrLn ""

let isErr res = case res of {
  (Ok _)  -> False,
  (Err _) -> True
} :: Result a -> Bool

putStrLn "We can even define a kind of monadic type of binding to sum types too:"
-- monadic bind for Result
let (>>=) res f = case res of {
  (Ok a) -> f a,
  (Err err) -> res
} :: Result a b -> (a -> Result c b) -> Result c b

putStrLn "Ok 10 >>= (\\n -> Ok (n + 1)) ="
print Ok 10 >>= (\n -> Ok (n + 1))

putStrLn ""
putStrLn "Let's implement safe subtraction on nats:"

let safeSub a b = case (a, b) of {
  (_, 0)         -> Ok a,
  (0, _)         -> Err "Nat underflow!",
  ((S n), (S m)) -> safeSub n m
} :: (Nat -> Nat -> Result Nat String)

putStrLn "safeSub 12 13 ="
print safeSub 12 13
putStrLn "safeSub 20 13 ="
print safeSub 20 13

putStrLn ""
putStrLn "Now we can sequence safe subtractions:"
putStrLn "(equivalent to 5 - (2 - 1)) safeSub 2 1 >>= safeSub 5 ="
print safeSub 2 1 >>= safeSub 5
putStrLn "(equivalent to 5 - (5 - 6)) safeSub 5 6 >>= safeSub 5 ="
print safeSub 5 6 >>= safeSub 5
