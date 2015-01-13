module ParseLib where

import Data.Char

infixr 3 <|>
infixl 4 <*>
infixl 5 <|
infixl 6 |>

type Parser a = String -> [(String, a)]

pure :: a -> Parser a
pure a = \s -> [(s, a)]

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
f <*> a = \s -> [(s1, g b) | (s0, g) <- f s, (s1, b) <- a s0]

(<|>) :: Parser a -> Parser a -> Parser a
a <|> b = \s -> take 1 (a s ++ b s)

(|>) :: Parser a -> Parser b -> Parser b
a |> b = pure (\a b -> b) <*> a <*> b

(<|) :: Parser a -> Parser b -> Parser a
a <| b = pure (\a b -> a) <*> a <*> b

guarded :: (a -> Bool) -> Parser a -> Parser a
guarded f p = \s -> [(s',a) | (s',a) <- p s, f a]

satisfy :: (Char -> Bool) -> Parser Char
satisfy f "" = []
satisfy f (c:s) = [(s, c) | f c]

char :: Char -> Parser Char
char c = satisfy ( == c )

string :: String -> Parser String
string "" = pure ""
string (c:cs) = pure (:) <*> char c <*> string cs

maybeOne :: Parser a -> Parser [a]
maybeOne p = pure (:) <*> p <*> pure [] <|> pure []

maybeSome :: Parser a -> Parser [a]
maybeSome p = atLeastOne p <|> pure []

atLeastOne :: Parser a -> Parser [a]
atLeastOne p = pure (:) <*> p <*> maybeSome p

atLeastOneSep :: Parser a -> Parser b -> Parser [a]
atLeastOneSep p s = pure (:) <*> p <*> maybeSome (s |> p)

exactlyOne :: Parser a -> Parser [a]
exactlyOne p = pure (:) <*> p <*> pure []

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

optSpaces :: Parser String
optSpaces = maybeSome $ satisfy isSpace

token :: Parser a -> Parser a
token p = p <| optSpaces

keyword :: String -> Parser String
keyword = token . string

parse :: Parser a -> String -> a
parse p s = case p s of
            []        -> error "Parse error"
            [("", x)] -> x
            [(s, x)]  -> error "Incomplete parse"
            _         -> error "Ambiguous parse. This shouldn't happen!"

