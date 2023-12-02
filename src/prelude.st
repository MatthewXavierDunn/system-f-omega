assume (Int :: *)
assume (Char :: *)

assume (Unit :: *)
assume (Unit :: Unit)

assume (Pair :: * -> * -> *)
assume (Pair :: a -> b -> (a, b))

assume (Nat :: *)
assume (Z :: Nat)
assume (S :: Nat -> Nat)

assume (List :: * -> *)
assume (Cons :: a -> List a -> List a)
assume (Nil :: List a)

assume (Bool :: *)
assume (True :: Bool)
assume (False :: Bool)

assume (Maybe :: * -> *)
assume (Just :: a -> Maybe a)
assume (Nothing :: Maybe a)

assume (Either :: * -> * -> *)
assume (Left :: a -> Either a b)
assume (Right :: b -> Either a b)

assume (Ordering :: *)
assume (LT :: Ordering)
assume (EQ :: Ordering)
assume (GT :: Ordering)

let (:) a as = Cons a as :: a -> List a -> List a

let (+) a b = case a of {
  0     -> b,
  (S n) -> n + (S b)
} :: (Nat -> Nat -> Nat)

let (-) a b = case (a, b) of {
  (_, 0)         -> a,
  (0, _)         -> 0,
  ((S n), (S m)) -> n - m
} :: (Nat -> Nat -> Nat)

let (*) a b = case b of {
  0     -> 0,
  (S n) -> (a * n) + a
} :: (Nat -> Nat -> Nat)

let (&&) a b = case (a,b) of {
  (True, True) -> True,
  _            -> False
} :: (Bool -> Bool -> Bool)

let (||) a b = case (a,b) of {
  (False, False) -> False,
  _              -> True
} :: (Bool -> Bool -> Bool)

let not a = case a of {
  True  -> False,
  False -> True
} :: (Bool -> Bool)

let just m a = case m of {
  (Just x) -> x,
  Nothing  -> a
} :: (Maybe a -> a -> a)

let maybe b f m = case m of {
  (Just a) -> f a,
  Nothing  -> b
} :: (b -> (a -> b) -> Maybe a -> b)

let either f g e = case e of {
  (Left a)  -> f a,
  (Right b) -> g b
} :: ((a -> c) -> (b -> c) -> Either a b -> c)

-- partial functions!
let head (x:_) = x :: (List a -> a)
let tail (_:xs) = xs :: (List a -> List a)

let last xs = case xs of {
  [x]    -> x,
  (x:xs) -> last xs
} :: (List a -> a)

let init xs = case xs of {
  [x]    -> [],
  (x:xs) -> Cons x (init xs)
} :: (List a -> List a)

let null xs = case xs of {
  [] -> True,
  _  -> False
} :: (List a -> Bool)

let take n xs = case n of {
  0     -> [],
  (S n) ->
    case xs of {
      (x:xs) -> Cons x (take n xs),
      []     -> []
    }
} :: (Nat -> List a -> List a)

let listToMaybe xs = case xs of {
  (x:_) -> Just x,
  []    -> Nothing
} :: (List a -> Maybe a)

let map f xs = case xs of {
  (x:xs) -> Cons (f x) (map f xs),
  []     -> []
} :: ((a -> b) -> List a -> List b)

let (++) as bs = case as of {
  []     -> bs,
  (a:as) -> Cons a (as ++ bs)
} :: (List a -> List a -> List a)

let filter p xs = case xs of {
  []     -> [],
  (x:xs) ->
    case p x of {
      True  -> Cons x (filter p xs),
      False -> filter p xs
    }
} :: ((a -> Bool) -> List a -> List a)

let length xs = case xs of {
  (_:xs) -> S (length xs),
  []     -> 0
} :: (List a -> Nat)

let sum xs = case xs of {
  (x:xs)      -> x + (sum xs),
  []          -> 0
} :: (List Nat -> Nat)

let fst (a, _) = a :: ((a, b) -> a)
let snd (_, b) = b :: ((a, b) -> b)

let curry f a b = f (a, b) :: (((a, b) -> c) -> a -> b -> c)
let uncurry f (a, b) = f a b :: ((a -> b -> c) -> (a, b) -> c)

let swap (a, b) = (b, a) :: ((a, b) -> (b, a))

-- infinite expressions
let repeat n = (n : (repeat n)) :: a -> List a

let replicate n x = take n (repeat x) :: Nat -> a -> List a

let cycle es = es ++ cycle es :: List a -> List a

let zip xs ys = case (xs, ys) of {
    ((x:xs), (y:ys)) -> Cons (x, y) (zip xs ys),
    _ -> []
} :: List a -> List b -> List ((a, b))
