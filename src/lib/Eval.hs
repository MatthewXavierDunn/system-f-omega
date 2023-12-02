module Eval where

import AST
import Common.Types
import Env
import Data.List (find)
import Data.Maybe (isJust, isNothing)
import Debug.Trace

type EvalResult = Either String Expression

type Binding = (Identifier, Expression)

evaluateExpression :: Env -> TEExpression -> Either String TEExpression
evaluateExpression = toNormalForm

substitute :: Identifier -> TEExpression -> TEExpression -> TEExpression
substitute name withExpression = sub
  where
    sub :: TEExpression -> TEExpression
    -- only replace if name matches
    sub (TEVar name')
      | name == name' = withExpression
    -- if free variable shadows name of sub-name, it is a different variable
    sub (TELam pattern expression)
      | name `notElem` flattenPattern pattern = TELam pattern (sub expression)
    sub (TEApp expression expression') = sub expression `TEApp` sub expression'
    sub (TECas expression arms) = sub expression `TECas` subArms arms
    sub expression = expression

    subArms :: [(Pattern, TEExpression)] -> [(Pattern, TEExpression)]
    subArms ((p, e) : rest)
      | name `notElem` flattenPattern p = (p, sub e) : subArms rest
    subArms arms = arms

betaRedux :: Env -> TEExpression -> Either String (TEExpression, Bool)
betaRedux env (TEApp (TELam pattern inExpression) withExpression) = do
  case unifies env pattern withExpression of
    Right subs ->
      Right (foldl (\acc (name, expression) -> substitute name expression acc) inExpression subs, True)
    Left True -> do
      (withExpression', didReduce) <- betaRedux env withExpression
      if didReduce
        then
          Right (TEApp (TELam pattern inExpression) withExpression', True)
        else
          Left ("Partial function, could not match " ++ pprint withExpression)
    Left False -> do
      (withExpression', didReduce) <- betaRedux env withExpression
      Right (TEApp (TELam pattern inExpression) withExpression', didReduce)
betaRedux env (TEApp expression expression') = do
  (left, didReduce) <- betaRedux env expression
  if didReduce
    then
      Right (TEApp left expression', True)
    else do
      (right, didReduce) <- betaRedux env expression'
      Right (TEApp left right, didReduce)
betaRedux env (TEVar name) =
  case getVar name env of
    Just expression ->
      Right (expression, True)
    Nothing ->
      Right (TEVar name, False)
betaRedux env (TECas e arms) = do
  case attemptMatch e arms of
    Right matchedArm ->
      Right (matchedArm, True)
    Left True -> do
      (e', didReduce) <- betaRedux env e
      if didReduce
        then
          Right (TECas e' arms, True)
        else
          Left ("No match arm found matching " ++ pprint e')
    Left False -> do
      (e', didReduce) <- betaRedux env e
      Right (TECas e' arms, didReduce)
  where
    attemptMatch :: TEExpression -> [(Pattern, TEExpression)] -> Either Bool TEExpression
    attemptMatch _ [] = Left True
    attemptMatch withExpression ((pattern, inExpression) : rest) =
      case unifies env pattern withExpression of
        Right subs ->
          Right $ foldl (\acc (name, expression) -> substitute name expression acc) inExpression subs
        Left True ->
          attemptMatch withExpression rest
        Left False ->
          Left False
betaRedux _ expression = Right (expression, False)

unifies :: Env -> Pattern -> TEExpression -> Either Bool [(Identifier, TEExpression)]
unifies env (PVar name) e
  | isConstructor name env =
    case e of
      (TEVar name') | name == name' ->
        Right []
      _ ->
        case firstApp e of
          (TEVar name') | isConstructor name' env ->
            Left True
          _ ->
            Left False
  | otherwise = Right [(name, e)]
  where
    firstApp (TEApp t1 t2) = firstApp t1
    firstApp e = e
unifies env (PApply p1 p2) (TEApp t1 t2) = do
  bindings <- unifies env p1 t1
  bindings' <- unifies env p2 t2
  Right (bindings ++ bindings')
unifies _ PAny _ = Right []
unifies _ _ _ = Left True

toNormalForm :: Env -> TEExpression -> Either String TEExpression
toNormalForm env expression = do
  (simplified, didReduce) <- betaRedux env expression
  -- prevent expressions that reduce to themselves from infinitely recursing
  if didReduce
    then toNormalForm env simplified
    else Right simplified
