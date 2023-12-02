module Main where

import Test.HUnit
import System.Exit
import Test.QuickCheck
import AST
import Env
import Check
import Data.Either
import Eval

-- \a -> a (The I combinator)
identityExpression :: Expression
identityExpression = Lam (PVar "a") (Var "a")

-- a -> a
identityType :: ConcreteType
identityType = CTArrow (CTVar "a") (CTVar "a")

-- \a b -> a (The K combinator)
constExpression :: Expression
constExpression = Lam (PVar "a") (Lam (PVar "b") (Var "a"))

-- a -> b -> a
constType :: ConcreteType
constType = CTArrow (CTVar "a") (CTArrow (CTVar "b") (CTVar "a"))

-- \x y z -> x z (y z) (The S combinator)
sExpression :: Expression
sExpression = Lam (PVar "x") (Lam (PVar "y") (Lam (PVar "z") (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))

-- (a -> b -> a) -> (a -> b) -> a -> a
sType :: ConcreteType
sType = CTArrow (CTArrow (CTArrow (CTVar "a") (CTVar "b")) (CTVar "a")) (CTArrow (CTArrow (CTVar "a") (CTVar "b")) (CTArrow (CTVar "a") (CTVar "a")))

-- if a variable is assigned with a type, it can be inferred that it has that type
propAnn :: String -> String -> Bool
propAnn n t = infer (setType n (CTVar t) (setKind t KTType baseEnv)) (Ann (Var n) (CTVar t)) == Right (CTVar t)

-- if a variable is assigned with a type, it can be checked against that type
propCheck :: String -> String -> Bool
propCheck n t = isRight $ check (setType n (CTVar t) (setKind t KTType baseEnv)) (Ann (Var n) (CTVar t)) (CTVar n)

-- identity function when applied to a value gives back that value
propIdentity :: String -> Bool
propIdentity n =
  let erasedExpression = eraseType (App identityExpression (Var n)) in
  evaluateExpression baseEnv erasedExpression == Right (TEVar n)

-- const function when applied to two values gives back the first value
propConst :: String -> String -> Bool
propConst a b =
  let erasedExpression = eraseType (App (App constExpression (Var a)) (Var b)) in
  evaluateExpression baseEnv erasedExpression == Right (TEVar a)

-- SK combinator is equivalent to False in lambda calculus, i.e. gives back its second argument when given two arguments
propSK :: String -> String -> Bool
propSK a b =
  let erasedExpression = eraseType (App (App (App sExpression constExpression) (Var a)) (Var b)) in
  evaluateExpression baseEnv erasedExpression == Right (TEVar b)

-- checking that (\a -> a) :: (a -> a)
testCheckIdentity :: Test
testCheckIdentity = TestCase (assert (isRight (check baseEnv identityExpression identityType)))

-- checking that (\a b -> a) :: (a -> b -> a)
testCheckConst :: Test
testCheckConst = TestCase (assert (isRight (check baseEnv constExpression constType)))

-- checking that S :: (a -> b -> a) -> (a -> b) -> a -> a
testCheckSK :: Test
testCheckSK = TestCase (assert (isRight (check baseEnv sExpression sType)))

main :: IO ()
main = do

  quickCheck propAnn
  quickCheck propCheck
  quickCheck propIdentity
  quickCheck propConst
  quickCheck propSK

  runTestTTAndExit (TestList [testCheckIdentity, testCheckConst, testCheckSK])
