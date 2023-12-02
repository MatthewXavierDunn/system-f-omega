{-# LANGUAGE ScopedTypeVariables #-}
module REPL where

import Common.State
import Control.Monad.State
import GHC.IO.Handle
import System.IO
import Text.Parsec hiding (try)
import AST
import Parser
import System.Exit
import Check
import Env
import Common.Types
import Eval
import Control.Exception
import System.IO.Error (catchIOError)

helpText :: String
helpText =
  "List of commands: Any command may be abbreviated to :c where\n"
    ++ "c is the first character in the full name of the command.\n\n"
    ++ "<expr>                  evaluate expression\n"
    ++ "let <val> = <expr>      define variable\n"
    ++ "assume <var> :: <expr>  assume variable\n\n"
    ++ ":type <expr>            print type of expression\n"
    ++ ":browse                 browse names in scope\n"
    ++ ":load <file>            load program from file\n"
    ++ ":quit                   exit interpreter\n"
    ++ ":help, :?               display this list of commands\n"

evalTLExpression :: TLExpression -> EnvStateT (Either String (TEExpression, ConcreteType))
evalTLExpression (Let name expression) = do
  env <- get
  case typeCheckTLExpression env (Let name expression) of
    Left err ->
      return $ Left ("TypeError: " ++ err)
    Right ct -> do
      modify (setType name ct)
      let typeErasedExpression = eraseType expression
      modify (setVar name typeErasedExpression)
      return $ Right (typeErasedExpression, ct)
evalTLExpression (Assume as) = do
  mapM_ (modify . makeAssumption) as
  return $ Right (TEVar "Unit", CTVar "Unit")
  where
    makeAssumption :: (Identifier, Type) -> Env -> Env
    makeAssumption (n, TConc ct) = addConstructor n . setType n ct
    makeAssumption (n, TKind kt) = setKind n kt
evalTLExpression (Expression expression) = do
  env <- get
  case infer env expression of
    Left err ->
      return $ Left ("TypeError: " ++ err)
    Right ct -> do
      let typeErasedExpression = eraseType expression
      let evalResult = evaluateExpression env typeErasedExpression
      return $ case evalResult of
        Left err ->
          Left err
        Right normalForm ->
          Right (normalForm, ct)
evalTLExpression (PutStrLn s) = do
  liftIO $ putStrLn s
  return $ Right (TEVar "Unit", CTVar "Unit")
evalTLExpression (Print expression) = do
  res <- evalTLExpression (Expression expression)
  case res of
    Left err ->
      return $ Left err
    Right (normalForm, _) -> do
      liftIO $ putStrLn (pprint normalForm)
      return $ Right (TEVar "Unit", CTVar "Unit")

eval :: Command -> EnvStateT ()
eval Quit = liftIO exitSuccess
eval Help = liftIO $ putStrLn helpText
eval (Eval input) =
  case parse parseTLExpression "interactive" input of
    (Left err) ->
      liftIO $ print err
    (Right expression) -> do
      evalResult <- evalTLExpression expression
      case evalResult of
        (Left err) ->
          liftIO $ putStrLn err
        (Right (normalForm, ct)) -> do
          liftIO $ putStrLn (pprint normalForm ++ " :: " ++ pprint ct)

eval (Compile file) = do
  input <- liftIO $ try (readFile file)
  case input of
    Left (err::IOException) ->
      liftIO $ putStrLn ("Could not open file " ++ file)
    Right input -> do
      liftIO $ putStrLn ("loading " ++ show file)
      case parse (manyTill parseTLExpression eof) file input of
        Left err ->
          liftIO $ print err
        Right expressions -> do
          loop expressions
          where
            loop :: [TLExpression] -> EnvStateT ()
            loop [] = return ()
            loop (statement : rest) = do
              env <- get
              case typeCheckTLExpression env statement of
                (Left err) ->
                  liftIO $ putStrLn ("TypeError: " ++ err)
                (Right _) -> do
                  evalResult <- evalTLExpression statement
                  case evalResult of
                    (Left err) ->
                      liftIO $ putStrLn err
                    (Right _) -> do
                      return ()
              loop rest
eval (TypeOf input) =
  case parse parseExpression "interactive" input of
    (Left err) ->
      liftIO $ print err
    (Right expression) -> do
      env <- get
      case infer env expression of
        (Left err) ->
          liftIO $ putStrLn ("TypeError: " ++ err)
        (Right typeExpression) -> do
          liftIO $ putStrLn (pprint (eraseType expression) ++ " :: " ++ pprint typeExpression)
eval Browse = do
  ts <- gets types
  liftIO $ putStrLn (unlines (map (\(name, typeExpression) -> name ++ " :: " ++ pprint typeExpression) ts))
eval NoOp =
  return ()

interpret :: String -> EnvStateT ()
interpret input =
  case parse parseCommand "stdin" input of
    Left err -> liftIO $ print err
    Right command -> eval command

initRepl :: EnvStateT ()
initRepl = do
  eval (Compile "prelude.st")
  repl

repl :: EnvStateT ()
repl = do
  liftIO $ putStr "Fω> "
  liftIO $ hFlush stdout
  input <- liftIO getLine
  interpret input
  repl
