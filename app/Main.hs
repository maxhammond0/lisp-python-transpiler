{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Bool
import Data.List
import System.Environment

type Name = String

data Expr
  = ATOM Atom
  | LIST [Expr]
  deriving (Show, Eq, Read)

data Atom
  = Int Int
  | Symbol Name
  deriving (Show, Eq, Read)

newtype Parser a
  = Parser (ParseString -> Either ParseError (a, ParseString))

data ParseString
  = ParseString Name (Int, Int) String

data ParseError
  = ParseError ParseString Error

type Error = String

instance Functor Parser where
  fmap f (Parser parser) =
    Parser (fmap (first f) . parser)

instance Applicative Parser where
  pure x = Parser (\str -> Right (x, str))
  (Parser p1) <*> (Parser p2) =
    Parser $
      \str -> do
        (f, rest) <- p1 str
        (x, rest') <- p2 rest
        pure (f x, rest')

instance Alternative Parser where
  empty = Parser (`throwErr` "Failed consuming input")
  (Parser p1) <|> (Parser p2) =
    Parser $
      \pstr -> case p1 pstr of
        Right result -> Right result
        Left _ -> p2 pstr

instance Monad Parser where
  (Parser p1) >>= f =
    Parser $
      \str -> case p1 str of
        Left err -> Left err
        Right (rs, rest) ->
          case f rs of
            Parser parser -> parser rest

runParser :: String -> String -> Parser a -> Either ParseError (a, ParseString)
runParser name str (Parser parser) = parser $ ParseString name (0,0) str

throwErr :: ParseString -> String -> Either ParseError a
throwErr ps@(ParseString name (row, col) _) errMsg =
  Left $
    ParseError ps $
      unlines
        [ "*** " ++ name ++ ": " ++ errMsg,
          "* On row " ++ show row ++ ", column " ++ show col ++ "."
        ]

oneOf :: [Char] -> Parser Char
oneOf chars =
  Parser $ \case
    ps@(ParseString name (row, col) str) ->
      case str of
        [] -> throwErr ps "Cannot read character of empty string"
        (c : cs) ->
          if c `elem` chars
            then
              let (row', col')
                    | c == '\n' = (row + 1, 0)
                    | otherwise = (row, col + 1)
               in Right (c, ParseString name (row', col') cs)
            else
              throwErr ps $
                unlines
                  [ "Unexpected character "
                      ++ [c],
                    "Expecting one of: " ++ show chars
                  ]

optional :: Parser a -> Parser (Maybe a)
optional (Parser parser) =
  Parser $
    \pstr -> case parser pstr of
      Left _ -> Right (Nothing, pstr)
      Right (x, rest) -> Right (Just x, rest)

many :: Parser a -> Parser [a]
many parser = go []
  where go cs = (parser >>= \c -> go (c:cs)) <|> pure (reverse cs)

many1 :: Parser a -> Parser [a]
many1 parser =
  (:) <$> parser <*> Main.many parser

char :: Char -> Parser Char
char c = oneOf [c]

string :: String -> Parser String
string = traverse char

space :: Parser Char
space = oneOf " \n"

spaces :: Parser String
spaces = Main.many space

spaces1 :: Parser String
spaces1 = Main.many1 space

withSpaces :: Parser a -> Parser a
withSpaces parser =
  spaces *> parser <* spaces

parens :: Parser a -> Parser a
parens parser =
  withSpaces (char '(')
  *> withSpaces parser
  <* (spaces *> char ')')

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep parser = do
  first <- Main.optional parser
  rest <- Main.many (sep *> parser)
  pure $ maybe rest (:rest) first

parseExpr :: Parser Expr
parseExpr = fmap ATOM parseAtom <|> fmap LIST parseList

parseList :: Parser [Expr]
parseList = parens $ sepBy spaces1 parseExpr

parseAtom :: Parser Atom
parseAtom = parseSymbol <|> parseInt

parseSymbol :: Parser Atom
parseSymbol = fmap Symbol parseName

parseName :: Parser Name
parseName = do
  c <- oneOf ['a' .. 'z']
  cs <- Main.many $ oneOf $ ['a' .. 'z'] ++ "0123456789" ++ "_"
  pure (c:cs)

parseInt :: Parser Atom
parseInt = do
  sign <- Main.optional $ char '_'
  num <- many1 $ oneOf "0123456789"
  let result = read $ maybe num (:num) sign
  pure $ Int result

runExprParser :: Name -> String -> Either String Expr
runExprParser name str =
  case runParser name str (withSpaces parseExpr) of
    Left (ParseError _ errMsg) -> Left errMsg
    Right (result, _) -> Right result

printExpr :: Expr -> String
printExpr = printExpr' False 0

printAtom :: Atom -> String
printAtom = \case
  Symbol s -> s
  Int i -> show i

printExpr' :: Bool -> Int -> Expr -> String
printExpr' doindent level = \case
  ATOM a -> indent (bool 0 level doindent) (printAtom a)
  LIST (e:es) ->
    indent (bool 0 level doindent) $
      concat
        [ "("
        , printExpr' False (level + 1) e
        , bool "\n" "" (null es)
        , intercalate "\n" $ map (printExpr' True (level + 1)) es
        , ")"
        ]

indent :: Int -> String -> String
indent tabs e = concat (replicate tabs "  ") ++ e


parseCode src output = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src, output] -> parseCode src output
    [src] -> parseCode src "a.py"
    _ -> putStrLn "Uncorrect usage: ./prog <src> <output>"
