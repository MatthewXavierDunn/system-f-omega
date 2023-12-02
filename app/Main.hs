module Main where

import Control.Monad
import Control.Monad.State
import REPL (initRepl)
import Env

main :: IO ()
main = do
  putStrLn "Interpreter for ST Lambda."
  putStrLn "Type :? for help."
  void $ execStateT initRepl baseEnv
