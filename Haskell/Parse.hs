module Parse where

import ParseLib
import Syntax
import Data.Char (isAlphaNum)

keywords :: [String]
keywords = [ "if", "try",
             "then", "else",
             "or", "skip", "fail" ]


{- keyword :: String -> Parser String
keyword s = token $ \input -> 
    [(rest, a) | (rest, a) <- string s input,
        null rest || not (isAlphaNum (head rest))] -}

identifier :: Parser Char -> Parser String
identifier begin = token (guarded g (pure (:) <*> begin <*> maybeSome alphanum))
    where g s = s `notElem` keywords


ruleSet :: Parser a -> Parser [a]
ruleSet p = keyword "{" |> rules <| keyword "}"
    where rules  = rules' <| (keyword "," <|> pure "")
          rules' = pure (:) <*> p <*> maybeSome (keyword "," |> p)
