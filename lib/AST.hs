module AST where

import Common.Types
import Data.Bifunctor
import Data.List

-- | Pretty printing type class for more user-readable output
class PPrint a where
  pprint :: a -> String

-- | Interactive REPL only commands
data Command
  = TypeOf String
  | Browse
  | Compile String
  | Quit
  | Help
  | Eval String
  | NoOp
  deriving (Eq, Show)

-- | Top level expression
data TLExpression
  = Let Identifier Expression
  | Assume [(Identifier, Type)]
  | Expression Expression
  | PutStrLn String
  | Print Expression
  deriving (Eq, Show)

-- | General expressions of the language
data Expression
  = Ann Expression ConcreteType
  | Var Identifier
  | Lam Pattern Expression
  | App Expression Expression
  | Case Expression [(Pattern, Expression)]
  deriving (Eq, Show)

instance PPrint Expression where
  pprint e = pprint (eraseType e)

-- | Type erased expressions, once an expression is type checked, we can discard all type annotations
data TEExpression
  = TEVar Identifier
  | TELam Pattern TEExpression
  | TEApp TEExpression TEExpression
  | TECas TEExpression [(Pattern, TEExpression)]
  deriving (Eq, Show)

eraseType :: Expression -> TEExpression
eraseType (Ann e _) = eraseType e
eraseType (Var n) = TEVar n
eraseType (Lam n e) = TELam n (eraseType e)
eraseType (App e1 e2) = TEApp (eraseType e1) (eraseType e2)
eraseType (Case e arms) = TECas (eraseType e) (map (second eraseType) arms)

instance PPrint TEExpression where
  pprint (TEVar ['#', c]) = show c
  pprint (TEVar "Z") = "0"
  pprint (TEVar "Nil") = "[]"
  pprint (TEVar "Unit") = "()"
  pprint (TEVar name) =
    name
  pprint lambda@(TELam _ _) =
    let (patterns, expression) = flatten lambda
     in "(\\" ++ unwords (map pprint patterns) ++ " -> " ++ pprint expression ++ ")"
    where
      flatten :: TEExpression -> ([Pattern], TEExpression)
      flatten (TELam pattern expression) = first (pattern :) $ flatten expression
      flatten e = ([], e)
  pprint pair@(TEApp (TEApp (TEVar "Pair") _) _) =
    "(" ++ intercalate ", " (map pprint (flattenPair pair)) ++ ")"
    where
      flattenPair :: TEExpression -> [TEExpression]
      flattenPair (TEApp (TEApp (TEVar "Pair") a) b) = a : flattenPair b
      flattenPair e = [e]
  pprint nat@(TEApp (TEVar "S") rest) =
    case flattenNat nat of
      (True, es) -> show $ natToInt es
      (False, _) -> "S " ++ pprint rest
    where
      flattenNat :: TEExpression -> (Bool, [TEExpression])
      flattenNat (TEVar "Z") = (True, [])
      flattenNat (TEApp (TEVar "S") x) = let (isFlat, es) = flattenNat x in (isFlat, TEVar "S" : es)
      flattenNat e = (False, [e])

      natToInt :: [TEExpression] -> Integer
      natToInt [] = 0
      natToInt [TEVar "Z"] = 0
      natToInt (TEVar "S" : es) = 1 + natToInt es
      natToInt x = error (show x)
  pprint cons@(TEApp (TEApp (TEVar "Cons") _) _) =
    case consToList cons of
      (True, listChar@((TEVar ('#':_)) : _)) -> show $ listCharToString listChar
      (True, list) -> "[" ++ intercalate ", " (map pprint list) ++ "]"
      (False, list) -> intercalate " : " (map pprint list)
    where
      consToList :: TEExpression -> (Bool, [TEExpression])
      consToList (TEVar "Nil") = (True, [])
      consToList (TEApp (TEApp (TEVar "Cons") e) rest) = let (isFlat, es) = consToList rest in (isFlat, e : es)
      consToList e = (False, [e])

      listCharToString :: [TEExpression] -> String
      listCharToString ((TEVar ('#':c:_)) : rest) = c : listCharToString rest
      listCharToString _ = []
  pprint (TEApp e1 e2) =
    "(" ++ pprint e1 ++ " " ++ pprint e2 ++ ")"
  pprint (TECas expression arms) =
    "\n  case " ++ pprint expression ++ " of {\n    " ++ intercalate "\n    " (map (\(p, e) -> pprint p ++ " -> " ++ pprint e) arms) ++ "\n  }"

-- | Pattern matching AST for lambdas, and case expressions
data Pattern
  = PVar Identifier
  | PApply Pattern Pattern
  | PAny
  deriving (Eq, Show)

instance PPrint Pattern where
  pprint (PVar ['#', c]) = show c
  pprint (PVar "Z") = "0"
  pprint (PVar "Unit") = "()"
  pprint (PVar "Nil") = "[]"
  pprint (PVar n) = n
  pprint nat@(PApply (PVar "S") rest) =
    case flattenNat nat of
      (True, es) -> show $ natToInt es
      (False, _) -> "S " ++ pprint rest
    where
      flattenNat :: Pattern -> (Bool, [Pattern])
      flattenNat (PVar "Z") = (True, [])
      flattenNat (PApply (PVar "S") x) = let (isFlat, es) = flattenNat x in (isFlat, PVar "S" : es)
      flattenNat e = (False, [e])

      natToInt :: [Pattern] -> Integer
      natToInt [] = 0
      natToInt [PVar "Z"] = 0
      natToInt (PVar "S" : es) = 1 + natToInt es
      natToInt x = error (show x)
  pprint cons@(PApply (PApply (PVar "Cons") _) _) =
    case consToList cons of
      (True, listChar@((PVar ('#':_)) : _)) -> show $ listCharToString listChar
      (True, list) -> "[" ++ intercalate ", " (map pprint list) ++ "]"
      (False, list) -> intercalate " : " (map pprint list)
    where
      consToList :: Pattern -> (Bool, [Pattern])
      consToList (PVar "Nil") = (True, [])
      consToList (PApply (PApply (PVar "Cons") e) rest) = let (isFlat, es) = consToList rest in (isFlat, e : es)
      consToList e = (False, [e])

      listCharToString :: [Pattern] -> String
      listCharToString ((PVar ('#':c:_)) : rest) = c : listCharToString rest
      listCharToString _ = []
  pprint pair@(PApply (PApply (PVar "Pair") _) _) =
    "(" ++ intercalate ", " (map pprint (flattenPair pair)) ++ ")"
    where
      flattenPair :: Pattern -> [Pattern]
      flattenPair (PApply (PApply (PVar "Pair") a) b) = a : flattenPair b
      flattenPair e = [e]
  pprint (PApply p1 p2) = "(" ++ pprint p1 ++ " " ++ pprint p2 ++ ")"
  pprint PAny = "_"

flattenPattern :: Pattern -> [Identifier]
flattenPattern (PVar n) = [n]
flattenPattern (PApply p1 p2) = flattenPattern p1 ++ flattenPattern p2
flattenPattern PAny = []

data Type
  = TKind KindType
  | TConc ConcreteType
  deriving (Eq, Show)

data KindType
  = KTType
  | KTArrow KindType KindType
  deriving (Eq, Show)

data ConcreteType
  = CTVar Identifier
  | CTArrow ConcreteType ConcreteType
  | CTApply ConcreteType ConcreteType
  deriving (Eq, Show)

instance PPrint Type where
  pprint (TKind kt) = pprint kt
  pprint (TConc ct) = pprint ct

instance PPrint KindType where
  pprint KTType = "*"
  pprint (KTArrow kt1 kt2) = pprint kt1 ++ " -> " ++ pprint kt2

instance PPrint ConcreteType where
  pprint (CTVar "Unit") = "()"
  pprint (CTVar n) = n
  pprint (CTApply (CTVar "List") (CTVar "Char")) = "String"
  pprint pair@(CTApply (CTApply (CTVar "Pair") _) _) =
    "(" ++ intercalate "," (map pprint (flattenPair pair)) ++ ")"
    where
      flattenPair :: ConcreteType -> [ConcreteType]
      flattenPair (CTApply (CTApply (CTVar "Pair") a) b) = a : flattenPair b
      flattenPair e = [e]
  pprint (CTApply ct1 ct2) = "(" ++ pprint ct1 ++ " " ++ pprint ct2 ++ ")"
  pprint pair@(CTArrow _ _) =
    "(" ++ intercalate " -> " (map pprint (flattenArrow pair)) ++ ")"
    where
      flattenArrow :: ConcreteType -> [ConcreteType]
      flattenArrow (CTArrow a b) = a : flattenArrow b
      flattenArrow e = [e]
