putStrLn ""
putStrLn "Examples of Inductive Types"
putStrLn ""

assume (BTree :: * -> *)
assume (Node :: BTree a -> BTree a -> a -> BTree a)
assume (Leaf :: BTree a)

putStrLn "Making a Binary Tree:"
let testTree = Node (Node (Node Leaf Leaf 2) Leaf 0) (Node (Node Leaf Leaf 5) (Node (Node Leaf Leaf 6) Leaf 3) 4) 7
print testTree

let treeMap f tree = case tree of {
  (Node l r v) -> Node (treeMap f l) (treeMap f r) (f v),
  Leaf -> Leaf
} :: (a -> b) -> BTree a -> BTree b

putStrLn ""
putStrLn "Mapping over a Binary Tree:"
putStrLn "treeMap (+4) testTree ="
print treeMap (+4) testTree


putStrLn ""
putStrLn "Doubly linked lists:"
assume (DoublyLinkedList :: * -> *)
assume (DoublyLinkedList :: List a -> List a -> DoublyLinkedList a)

let dll = DoublyLinkedList [1,2,3,4] []
print dll

putStrLn ""
putStrLn "Going forward through the doubly linked list:"
let next (DoublyLinkedList (x:xs) (bs)) = DoublyLinkedList xs (x:bs) :: DoublyLinkedList a -> DoublyLinkedList a
putStrLn "next dll ="
print next dll
putStrLn "let two_in_dll = next (next dll) ="
print next (next dll)
let two_in_dll = next (next dll)

putStrLn ""
putStrLn "Going backward through the doubly linked list:"
let back (DoublyLinkedList (xs) (b:bs)) = DoublyLinkedList (b:xs) bs :: DoublyLinkedList a -> DoublyLinkedList a
putStrLn "back (two_in_dll)"
print back two_in_dll
putStrLn "back (back (two_in_dll))"
print back (back two_in_dll)

