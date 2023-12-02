putStrLn ""
putStrLn "Examples of Product Types"
putStrLn ""

assume (Tuple :: * -> * -> *)
assume (Tuple :: a -> b -> Tuple a b)

-- eliminators
let fstTuple (Tuple a b) = a :: Tuple a b -> a
let sndTuple (Tuple a b) = b :: Tuple a b -> b

putStrLn "Making a tuple:"
putStrLn "Tuple 1 'a' ="

let testTuple = Tuple 1 'a'
print testTuple

putStrLn ""
putStrLn "Getting the first element:"
putStrLn "fstTuple testTuple ="
print fstTuple testTuple

putStrLn ""
putStrLn "Getting the second element:"
putStrLn "sndTuple testTuple ="
print sndTuple testTuple

putStrLn ""
putStrLn "Actually, records are just product types"

assume (Person :: *)
assume (Person :: String -> Person)

assume (Pet :: *)
assume (Pet :: String -> Person -> Pet)

putStrLn ""
putStrLn "Making a pet with an owner:"
print Pet "Fido" (Person "Alice")
