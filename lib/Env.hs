module Env where

import AST
import Common.Types
import Map (Map)
import qualified Map
import Data.Char (chr)

data Env = Env
  { variables :: Map Identifier TEExpression,
    types :: Map Identifier ConcreteType,
    constructors :: [Identifier],
    kinds :: Map Identifier KindType
  }
  deriving (Show)

allChars :: [String]
allChars = [['#', chr n] | n <- [0..256]]

baseEnv :: Env
baseEnv =
  Env
    { variables = Map.empty,
      types = [(c, CTVar "Char") | c <- allChars],
      constructors = allChars,
      kinds = Map.empty
    }

setVar :: Identifier -> TEExpression -> Env -> Env
setVar n e env = env {variables = Map.insert n e (variables env)}

setType :: Identifier -> ConcreteType -> Env -> Env
setType n ct env = env {types = Map.insert n ct (types env)}

setKind :: Identifier -> KindType -> Env -> Env
setKind n ct env = env {kinds = Map.insert n ct (kinds env)}

getVar :: Identifier -> Env -> Maybe TEExpression
getVar n env = Map.get n (variables env)

getType :: Identifier -> Env -> Maybe ConcreteType
getType n env = Map.get n (types env)

getKind :: Identifier -> Env -> Maybe KindType
getKind n env = Map.get n (kinds env)

hasKind :: Identifier -> Env -> Bool
hasKind n env = Map.has n (kinds env)

addTypes :: [(Identifier, ConcreteType)] -> Env -> Env
addTypes rs env = env {types = Map.extend rs (types env)}

addConstructor :: Identifier -> Env -> Env
addConstructor n env = env { constructors = n : filter (/= n) (constructors env) }

isConstructor :: Identifier -> Env -> Bool
isConstructor n env = n `elem` constructors env
