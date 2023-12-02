putStrLn ""
putStrLn "Examples of Incorrect Types"
putStrLn ""

putStrLn "Trying to add a Char to a Nat:"
putStrLn "'a' + 2"
print 'a' + 2
putStrLn ""

putStrLn "Trying to map over a list using a non function:"
putStrLn "map 1 [1,2,3]"
print map 1 [1,2,3]
putStrLn ""

putStrLn "Trying to get the head of an empty list:"
putStrLn "head []"
print head []
putStrLn ""

putStrLn "Zipping a list to a non list:"
putStrLn "zip [1,2,3] ()"
print zip [1,2,3] ()
putStrLn ""

putStrLn "Trying to apply a non function to a value"
putStrLn "[1,2,3] 5"
print [1,2,3] 5
putStrLn ""

-- note that unefined is not a value in the language
putStrLn "Using an unbound value:"
putStrLn "undefined"
print undefined
putStrLn ""

putStrLn "Using a lambda that is unannotated:"
putStrLn "(\\a -> a) 1"
print (\a -> a) 1
putStrLn ""
