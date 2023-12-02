module Check where

import AST
import Common.Types
import Env
import Control.Monad

type TypeCheckResult = Either String
type TypingContext = [(Identifier, ConcreteType)]

typeSub :: ConcreteType -> Identifier -> ConcreteType -> ConcreteType
-- axiom (t-sub var1)
-- axiom (t-sub var2)
typeSub (CTVar alpha) alpha' tau
  | alpha' == alpha = tau
  | otherwise = CTVar alpha
-- axiom (t-sub arrow)
typeSub (CTArrow sigma upsilon) alpha tau =
  CTArrow (typeSub sigma alpha tau) (typeSub upsilon alpha tau)
typeSub (CTApply sigma upsilon) alpha tau =
  CTApply (typeSub sigma alpha tau) (typeSub upsilon alpha tau)

-- convenience function for applying multiple substitutions at once
typeSubAll :: ConcreteType -> [(Identifier, ConcreteType)] -> ConcreteType
typeSubAll = foldl (\sigma (n, tau) -> typeSub sigma n tau)

typeCheckTLExpression :: Env -> TLExpression -> TypeCheckResult ConcreteType
-- add annotation to typing context to allow typing of recursive functions
typeCheckTLExpression env (Let name ann@(Ann _ typeExpression)) = infer (setType name typeExpression env) ann
typeCheckTLExpression env (Let _ expression) = infer env expression
typeCheckTLExpression _ (Assume _) = return (CTVar "Unit")
typeCheckTLExpression env (Expression expression) = infer env expression
typeCheckTLExpression _ (PutStrLn _) = return (CTVar "Unit")
typeCheckTLExpression env (Print expression) = do
  infer env expression
  return (CTVar "Unit")

infer :: Env -> Expression -> TypeCheckResult ConcreteType
-- axiom (type var)
infer gamma (Var x) =
  getType x gamma <!> "No type found for " ++ x
-- axiom (type app)
infer gamma (App e1 e2) = do
  alpha <- infer gamma e1
  case alpha of
    CTArrow sigma tau -> do
      omega <- check gamma e2 sigma
      Right (typeSubAll tau omega)
    _ ->
      Left ("Could not apply " ++ pprint e2 ++ " to non function " ++ pprint e1 ++ " :: " ++ pprint alpha)
-- axiom (type term)
infer gamma (Ann e tau) = do
  void $ check gamma e tau
  Right tau
infer gamma (Case e arms) = do
  sigma <- infer gamma e
  taus <- mapM (checkArm sigma) arms
  (tau, _) <- unifyAll gamma [] taus <!> "Could not unify " ++ show (map pprint taus)
  Right tau
  where
    checkArm sigma (p, t) = do
      gamma_n <- pCheck gamma p sigma
      infer (addTypes gamma_n gamma) t
-- to make `infer` a total function
infer _ lambda@(Lam _ _) =
  Left ("Lambda abstraction " ++ pprint lambda ++ " must be annotated")

check :: Env -> Expression -> ConcreteType -> Either String TypingContext
-- axiom (type abs)
check gamma (Lam p e) ct = do
  case ct of
    CTArrow sigma tau -> do
      gamma' <- pCheck gamma p sigma
      check (addTypes gamma' gamma) e tau
    _ -> Left ("Could not match " ++ pprint (Lam p e) ++ " with " ++ pprint ct)
-- axiom (type term)
check gamma e tau = do
  tau' <- infer gamma e
  (_, omega) <- unifyAll gamma [] [tau, tau'] <!> pprint e ++ " :: " ++ pprint tau' ++ " does not match " ++ pprint tau
  Right omega

inferKind :: Env -> ConcreteType -> Either String KindType
inferKind gamma (CTVar x) =
  getKind x gamma <!> "No kind found for " ++ x
inferKind gamma (CTApply sigma tau) = do
  sigmaKind <- inferKind gamma sigma
  case sigmaKind of
    KTArrow k1 k2 -> do
      k1' <- inferKind gamma tau
      if k1' == k1
        then Right k2
        else Left ("Could not match " ++ pprint k1 ++ " to " ++ pprint k1')
    _ ->
      Left ("Could not apply " ++ pprint tau ++ " to kind " ++ pprint sigmaKind)
inferKind _ sigma = Left ("Could not infer kind of " ++ pprint sigma)

pCheck :: Env -> Pattern -> ConcreteType -> Either String TypingContext
-- special any
pCheck _ PAny _ = Right []
-- axiom (p-unify bind)
pCheck gamma (PVar x) sigma
  | not (isConstructor x gamma) = do
    Right [(x, sigma)]
-- axiom (p-unify pattern)
pCheck gamma p sigma = do
  (tau, gamma1) <- pInfer gamma p
  void $ unifyAll gamma [] [sigma, tau] <!> "Could not match pattern " ++ pprint p ++ " :: " ++ pprint tau ++ " with type " ++ pprint sigma
  Right gamma1

pInfer :: Env -> Pattern -> Either String (ConcreteType, TypingContext)
-- axiom (p-unify var)
pInfer gamma (PVar x)
  | isConstructor x gamma = do
    sigma <- getType x gamma <!> "Constructor " ++ x ++ " has no type"
    Right (sigma, [])
-- axiom (p-unify app)
pInfer gamma (PApply p1 p2) = do
  (p1Type, gamma1) <- pInfer gamma p1
  case p1Type of
    CTArrow sigma tau -> do
      gamma2 <- pCheck gamma p2 sigma
      Right (tau, gamma1 ++ gamma2)
    _ ->
      Left ("Could not apply pattern " ++ pprint p2 ++ " to non arrow type " ++ pprint p1Type)
pInfer _ p = Left ("Could not infer type of pattern " ++ pprint p)

unifyAll :: Env -> TypingContext -> [ConcreteType] -> Maybe (ConcreteType, TypingContext)
-- base case, succesful unification
unifyAll _ omega [sigma] = Just (sigma, omega)
-- axiom (unifyAll var1)
unifyAll env omega (CTVar x : CTVar x' : gamma)
  | x == x' = unifyAll env omega (CTVar x : gamma)
-- axiom (unifyAll apply)
unifyAll env omega (CTApply sigma1 tau1 : CTApply sigma2 tau2 : gamma) = do
  (sigma, omega1) <- unifyAll env omega [sigma1, sigma2]
  (tau, omega2) <- unifyAll env omega1 [tau1, tau2]
  unifyAll env omega2 (CTApply sigma tau : gamma)
-- axiom (unifyAll arrow)
unifyAll env omega (CTArrow sigma1 tau1 : CTArrow sigma2 tau2 : gamma) = do
  (sigma, omega1) <- unifyAll env omega [sigma1, sigma2]
  (tau, omega2) <- unifyAll env omega1 [tau1, tau2]
  unifyAll env omega2 (CTArrow sigma tau : gamma)
-- deal with forall
unifyAll env omega (CTVar n : tau : gamma)
  | not (hasKind n env) =
    unifyAll env ((n, tau) : omega) (tau : map (\sigma -> typeSub sigma n tau) gamma)
unifyAll env _ (CTVar a : CTVar b : _)
  | hasKind a env && hasKind b env =
    Nothing
unifyAll env omega (tau : CTVar x : gamma) =
  unifyAll env omega (CTVar x : tau : gamma)
-- failure case
unifyAll env _ _ = Nothing
