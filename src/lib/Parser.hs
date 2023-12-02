module Parser where

import AST
import Common.Parser
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import Common.Types

parseCommand :: Parser Command
parseCommand = do
  command <- try parseRealCommand <|> parseNoOp <|> (Eval <$> manyTill anyChar eof)
  eof
  return command
  where
    parseRealCommand :: Parser Command
    parseRealCommand = do
      whitespace
      void $ char ':'
      parseTypeOf <|> parseBrowse <|> parseLoad <|> parseQuit <|> parseHelp

    parseTypeOf :: Parser Command
    parseTypeOf = do
      void (try $ string "type") <|> void (try $ char 't')
      whitespace
      TypeOf <$> manyTill anyChar eof

    parseBrowse :: Parser Command
    parseBrowse = do
      void (try $ string "browse") <|> void (try $ char 'b')
      return Browse

    parseLoad :: Parser Command
    parseLoad = do
      void (try $ string "load") <|> void (try $ char 'l')
      whitespace
      file <- many1 anyChar <?> "file path"
      return $ Compile file

    parseQuit :: Parser Command
    parseQuit = do
      void (try $ string "quit") <|> void (try $ char 'q')
      return Quit

    parseHelp :: Parser Command
    parseHelp = do
      void (try $ string "help") <|> void (try $ char '?') <|> void (try $ char 'h')
      return Help

    parseNoOp :: Parser Command
    parseNoOp = do
      whitespace
      eof
      return NoOp

parseTLExpression :: Parser TLExpression
parseTLExpression = do
  whitespace
  statement <- parseLet <|> (Assume <$> parseAssume) <|> (Expression <$> parseExpression) <|> parsePutStrLn <|> parsePrint
  void (many newline) <|> eof
  return statement
  where
    parseLet :: Parser TLExpression
    parseLet = do
      reserved "let"
      variable <- identifier <|> parens operator
      patterns <- many parsePattern
      reservedOp "="
      expression <- parseExpression
      constructor <- pAnn <|> return id
      return $ Let variable (constructor (foldr Lam expression patterns))
      where
        pAnn :: Parser (Expression -> Expression)
        pAnn = do
          reserved "::"
          typeExpression <- parseConcreteType
          return (`Ann` typeExpression)

    parseAssume :: Parser [(Identifier, Type)]
    parseAssume = do
      reserved "assume"
      bindings <- many1 (parens parseBindings)
      void (many newline) <|> eof
      return bindings

    parseBindings :: Parser (Identifier, Type)
    parseBindings = do
      name <- identifier <|> parens operator
      reserved "::"
      t <- (TConc <$> parseConcreteType) <|> (TKind <$> parseKindType)
      return (name, t)

    parseKindType :: Parser KindType
    parseKindType = do
      chainr1 p op
      where
        p = do
          reserved "*"
          return KTType
        op = do
          reservedOp "->"
          return KTArrow

    parsePutStrLn :: Parser TLExpression
    parsePutStrLn = do
      reserved "putStrLn"
      PutStrLn <$> stringLiteral

    parsePrint :: Parser TLExpression
    parsePrint = do
      reserved "print"
      Print <$> parseExpression

parseExpression :: Parser Expression
parseExpression = do
  chainl1 parseApp parseOp <|> parsePartialFn
  where
    parseApp :: Parser Expression
    parseApp = do
      ts <- many1 parseTerm
      return $ foldl1 App ts

    parseOp = do
      op <- operator
      return $ \x y -> App (App (Var op) x) y

    parseTerm :: Parser Expression
    parseTerm = do
      try (parens parseMaybeAnnExpression) <|> parseLambda <|> parseCase <|> (Var <$> identifier <|> (Var <$> try (parens operator))) <|> parseString <|> parseNat <|> parseList <|> parseLiteral <|> parseTuple

    parseMaybeAnnExpression = do
      expression <- parseExpression
      (Ann expression <$> (reservedOp "::" >> parseConcreteType)) <|> return expression

    parsePartialFn = do
      op <- operator
      ts <- many parseExpression
      let expression = foldl App (Var op) ts
      return expression

    parseLambda :: Parser Expression
    parseLambda = do
      reservedOp "\\"
      patterns <- many1 parsePattern
      reservedOp "->"
      term <- parseExpression
      return $ foldr Lam term patterns

    parseTuple :: Parser Expression
    parseTuple = do
      ts <- try (parens (commaSep parseExpression))
      return $ case ts of
        [] -> Var "Unit"
        _ -> foldr1 (App . App (Var "Pair")) ts

    parseList :: Parser Expression
    parseList =
      foldr (App . App (Var "Cons")) (Var "Nil") <$> brackets (commaSep parseExpression)

    parseNat :: Parser Expression
    parseNat =
      intToNat <$> natLiteral
      where
        intToNat 0 = Var "Z"
        intToNat n = App (Var "S") (intToNat (n - 1))

    parseString :: Parser Expression
    parseString =
      foldr ((App . App (Var "Cons")) . (\c -> Var ['#', c])) (Var "Nil") <$> stringLiteral

    parseLiteral :: Parser Expression
    parseLiteral = do
      c <- charLiteral
      return $ Var ['#', c]

    parseCase :: Parser Expression
    parseCase = do
      reserved "case"
      expression <- parseExpression
      reserved "of"
      arms <- braces $ commaSep1 parseArm
      return $ Case expression arms
      where
        parseArm = do
          pattern <- parsePattern
          reservedOp "->"
          arm <- parseExpression
          return (pattern, arm)

parsePattern :: Parser Pattern
parsePattern = do
  try (parens parsePApp) <|> parsePTerm
  where
    parsePApp = do
      ps <- many1 parsePTerm
      return $ foldl1 PApply ps

    parsePTerm = do
      try (parens parsePattern) <|> (PVar <$> identifier) <|> parsePCons <|> parsePList <|> parsePTuple <|> parsePNat <|> parsePChar

    parsePCons = do
      try $ parens (chainr1 parsePattern parseConsOp)

    parseConsOp = do
      reservedOp ":"
      return $ PApply . PApply (PVar "Cons")

    parsePList = do
      ps <- brackets (commaSep parsePattern)
      return $ foldr (PApply . PApply (PVar "Cons")) (PVar "Nil") ps

    parsePTuple = do
      ps <- try (parens (commaSep parsePattern))
      return $ case ps of
        [] -> PVar "Unit"
        _ -> foldr1 (PApply . PApply (PVar "Pair")) ps

    parsePChar = do
      c <- charLiteral
      return $ PVar ['#', c]

    parsePNat = do
      intToNat <$> natLiteral

    intToNat 0 = PVar "Z"
    intToNat n = PApply (PVar "S") (intToNat (n - 1))

parseConcreteType :: Parser ConcreteType
parseConcreteType = do
  chainr1 parseCTApp parseOp
  where
    parseCTApp :: Parser ConcreteType
    parseCTApp = do
      ts <- many1 parseCTTerm
      return $ foldr1 CTApply ts

    parseCTTerm = do
      try (parens parseConcreteType) <|> parseTypeVar <|> parseTypeTuple

    parseOp = do
      reservedOp "->"
      return CTArrow

    -- parseForall = do
    --   reserved "forall"
    --   names <- many1 identifier
    --   reservedOp "."
    --   ct <- parseConcreteType
    --   return $ foldr CTForall ct names

    parseTypeVar :: Parser ConcreteType
    parseTypeVar = do
      name <- identifier
      return $ case name of
        "String" -> CTApply (CTVar "List") (CTVar "Char")
        _ -> CTVar name

    parseTypeTuple :: Parser ConcreteType
    parseTypeTuple = do
      ts <- try (parens (commaSep parseConcreteType))
      return $ case ts of
        [] -> CTVar "Unit"
        _ -> foldl1 (CTApply . CTApply (CTVar "Pair")) ts
