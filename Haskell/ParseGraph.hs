module ParseGraph where

import Data.Char (toLower, isDigit)
import Data.Maybe (isJust, fromJust)
import Control.Monad (guard)
import Text.Parsec
import GPSyntax
import List

type Parser a  =  Parsec String () a

keyword :: String -> Parser ()
keyword s  =  try $ do { string s ; notFollowedBy letter ; spaces } 

symbol :: String -> Parser ()
symbol  s  =  try $ do { string s ; spaces }

comment :: Parser ()
comment  =  do { string "//" ; skipMany (noneOf "\n") ; newline ; return () }

manyCommented :: Parser a -> Parser [a]
manyCommented p  =  do { xss <- many $ do {comment ; spaces ; return []}
                                   <|> do {x <- p ; return [x]} ;
                         return $ concat xss }

many1Commented :: Parser a -> Parser [a]
many1Commented p  =  do { xs <- manyCommented p ; guard $ nonEmpty xs ; return xs }
 
hostGraph :: Parser AstHostGraph
hostGraph  =  do { many comment ; spaces ;
                   symbol "[" ; ns <- manyCommented hostNode ;
                   symbol "|" ; es <- manyCommented hostEdge ;
                   symbol "]" ; return $ AstHostGraph ns es }

hostNode :: Parser HostNode
hostNode  =  do { symbol "(" ;
                  n <- lowerIdent ; r <- root ; symbol "," ; l <- hostLabel ;
                  symbol ")" ; return $ HostNode n r l }

hostEdge :: Parser HostEdge
hostEdge  =  do { symbol "(" ; _ <- lowerIdent ; symbol "," ;
                  s <- lowerIdent ; symbol "," ; t <- lowerIdent ; symbol "," ;
                  l <- hostLabel ;
                  symbol ")" ; return $ HostEdge s t l }

hostLabel :: Parser HostLabel
hostLabel  =  do { as <- hostList ; c <- hostColour ; return $ HostLabel as c }

hostList :: Parser [HostAtom]
hostList  =  do { keyword "empty" ; return [] } <|>  sepBy1 value (symbol ":")

hostColour :: Parser Colour
hostColour  =  do { symbol "#" ; c <- many1 lower ;
                    let { hc = lookup c hostColours } ;
                    guard $ isJust hc ; return $ fromJust hc }
          <|>  return Uncoloured

value :: Parser HostAtom
value = intLit <|> strLit <|> charLit

intLit :: Parser HostAtom
intLit  =  do { ds <- many1 digit ; spaces ; return $ Int (read ds) }

charLit :: Parser HostAtom
charLit  =  do { c <- between (char '\'') (char '\'') gpChar ;
                 spaces ; return $ Chr c }

strLit :: Parser HostAtom
strLit  =  do { s <- between (char '"') (char '"') (many gpChar) ;
                spaces ; return $ Str s }

gpChar :: Parser Char
gpChar  =  oneOf gpChars

identifier :: Parser Char -> Parser String
identifier first  =  do { c <- first ; cs <- many gpChar ; let { s = c:cs } ;
                          guard $ map toLower s `notElem` keywords ; return s }

lowerIdent :: Parser String
lowerIdent  =  do { id <- identifier lower ; spaces; return id }

root :: Parser Bool
root  =  option False $ do { symbol "(R)" ; return True}


