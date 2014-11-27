module Parser where

import Text.Parsec
import Text.Parsec.String
import Data.Char (isLower)

import LispValues

readExpr :: String -> Either ParseError Expr
readExpr e = parse parseExpr "" e

parseExpr :: Parser Expr
parseExpr =     try parseFunctionDef
            <|> try parseLambda
            <|> try parseDefine
            <|> try parseIf
            <|> try parseEval
            <|> parseQuoted
            <|> parseNil
            <|> try parseNumber
            <|> parseString
            <|> parseSymbol 
            <|> parseList
            <|> parseBool

parseNumber :: Parser Expr
parseNumber = do
              sign   <- parseSign
              numStr <- many1 digit
              decimals <- parseDecimalPlaces <|> return 0
              return (Number $ sign * (read numStr + decimals))

parseSymbol :: Parser Expr
parseSymbol = do
              sym <- parseName <|> fmap (:[]) (oneOf "+*/-")
              return $ Symbol sym

parseString :: Parser Expr
parseString = do
              _ <- char '"'
              s <- many $ noneOf "\""
              _ <- char '"'
              return $ String s

parseList :: Parser Expr
parseList = do
            _ <- char '('
            list <- sepBy parseExpr spaces
            _ <- char ')'
            return $ List list

parseDefine :: Parser Expr
parseDefine = do
    _   <- char '('
    _   <- string "def"
    _   <- char ' '
    sym <- parseName
    _   <- char ' '
    e   <- parseExpr
    _   <- char ')'
    return $ Define sym e

parseLambda :: Parser Expr
parseLambda = do
    _        <- char '('
    _        <- string "lambda"
    _        <- char ' '
    _        <- char '('
    captured <- sepBy (many1 letter) (char ' ')
    _        <- char ')'
    _        <- char ' '
    expr     <- parseExpr
    _        <- char ')'
    return $ Lambda captured expr

parseFunctionDef :: Parser Expr
parseFunctionDef = do
    _        <- string "(def ("
    fn       <- parseName
    _        <- char ' '
    captured <- sepBy (many1 letter) (char ' ')
    _        <- string ") "
    expr     <- parseExpr
    _        <- char ')'
    return $ sugaredLambda fn captured expr
    where
        sugaredLambda f cap e = Define f (Lambda cap e)

parseNil :: Parser Expr
parseNil = do
    _ <- string "Nil"
    return (Quoted (List []))

parseBool :: Parser Expr
parseBool = do
    _ <- char '#'
    b <- char 't' <|> char 'f'
    return $ case b of
                't' -> Bool True
                _   -> Bool False

parseName :: Parser String
parseName = do
    first <- satisfy isLower
    name  <- many $ satisfy isLower <|> char '-'
    qm    <- optionMaybe (char '?')
    return $ case qm of
                Just _  -> (first:name) ++ "?"
                Nothing -> first:name

parseIf :: Parser Expr
parseIf = do
    _ <- string "(if "
    c <- parseExpr
    _ <- char ' '
    a <- parseExpr
    _ <- char ' '
    b <- parseExpr
    _ <- char ')'
    return $ If c a b

parseQuoted :: Parser Expr
parseQuoted = do
    _ <- char '\''
    e <- parseExpr
    return (Quoted e)

parseEval :: Parser Expr
parseEval = do
    _ <- string "(eval "
    e <- parseExpr
    _ <- char ')'
    return (Eval e)
    
parseSign :: Num a => Parser a
parseSign = do
    s <- optionMaybe (char '-')
    return $ case s of
        Just '-' -> -1
        _        -> 1

parseDecimalPlaces :: Parser Double
parseDecimalPlaces = do
    _    <- char '.'
    nums <- many1 digit
    return (read ("0." ++ nums))
