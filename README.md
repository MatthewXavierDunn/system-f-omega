# Simply Typed Lambda Calculus

An interpretor for an extension of the simply typed lambda calculus, more
formally a variation of System F $\omega$ from the lambda cube; that is to say
that there exists type polymorphism and types that depend on other types
(different from dependent typing) within the calculus.

However, the language itself does not follow the absolute strict definition of
System F $\omega$, only borrowing some of the redexes and type inference rules.

## Usage

After cloning this repository, the interpreter can be run using `cabal run`.
This will deal with installing all the necessary dependencies and building the
interpreter, before starting it.

Once you are in the interpreter you should be presented with the following
prompt:

```
Interpreter for System Fω.
Type :? for help.
loading "prelude.st"
Fω>
```

The interpreter will load the standard library through the `prelude.st` file.
This is located in the source directory of the repository.

To get the list of file commands and their usage, use `:?` or `:h`.

## Syntax

### Type Declaration

Types are declared using the following syntax:

```
assume (Nat :: *)
```

This creates the type `Nat` which has the type, `Type`. However, there is
currently no way to build a something of type `Nat`.

However, using a similar syntax, we can create a type constructor for `Nat`.

```
assume (Z :: Nat)
assume (S :: Nat -> Nat)
```

This says we have the constructor `Z` that gives us a `Nat` and a function
called `S` that takes a `Nat` and gives back a `Nat`.

In fact, these type constructors can be polymorphic, and also higher-kinded.
For example, the `Maybe` type:

```
assume (Maybe :: * -> *)
assume (Just :: a -> Maybe a)
assume (Nothing :: Maybe a)
```

This tells use that `Maybe` takes a type and gives back a type. `Just` is a
constructor for `Maybe` that takes some arbitrary `a`, and gives back a `Maybe`
monomorphised with `a`. `Nothing` is also a constructor for some `Maybe a`.
Since `Nothing` has an free type variable `a`, it can be coerced to any type
automatically.

## Type Annotations

Any expression can be optionally annotated. However, function declarations and
lambda abstractions must have a top-level type annotation.

```
Fω> True :: Bool
True :: Bool
Fω> 1 + 2 :: Nat
3 :: Nat
Fω> let id x = x :: a -> a
(\x -> x) :: (a -> a)
```

This is okay. However the following is not ok:

```
Fω> let id x = x
TypeError: Lambda abstraction (\x -> x) must be annotated
```

### Function and Variable Definition

Functions and variables are defined using the `let` key word.

```
Fω> let a = 1
1 :: Nat
Fω> :t a
a :: Nat
Fω> let twice f x = f (f x) :: (a -> a) -> a -> a
(\f x -> (f (f x))) :: ((a -> a) -> a -> a)
Fω> :t twice
twice :: ((a -> a) -> a -> a)
Fω> twice (+1) 2
4 :: Nat
```

### Pattern Matching

To unwrap type constructors, pattern matching is required. This is done in a
style similar to `Haskell` with `case` blocks:

```
let (+) a b = case a of {
  0     -> b,
  (S n) -> n + (S b)
} :: (Nat -> Nat -> Nat)

let not a = case a of {
  True  -> False,
  False -> True
} :: (Bool -> Bool)
```

Essentially, they consist of a variable that is being matched against, and a
sequence of match arms. Each match arm has some pattern, followed by a `->` and
then the match body. There is also variable binding that occurs in the match
arm.

### Lazy Evaluation

The interpreter uses leftmost outermost evaluation ordering so we can construct
data streams easily.

```
Fω> let repeat n = (n : (repeat n)) :: a -> List a
(\n -> ((: n) (repeat n))) :: (a -> (List a))
```

This function takes some value and repeats it infinitely in a `List`.
Obviously, trying to use this function on its own will cause the interpeter to
crash:

```
Fω> repeat 1

```

Using the `take` function, that takes `n` items from some `List`, we can
prevent this by ensuring there is a normal form to reach.

```
Fω> take 4 (repeat "hello there")
["hello there", "hello there", "hello there", "hello there"] :: (List String)
```
