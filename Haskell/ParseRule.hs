import ParseLib
import ParseGraph
import Data.List
import Data.String

gpKeywords :: [String]
gpKeywords = map fst gpColours ++
             ["main", "if", "try", "then", "else", "or", "skip", 
	      "fail", "int", "char", "string", "atom", "list",
              "interface", "where", "injective", "true", "false",
              "and", "not", "edge", "empty", "indeg", "outdeg",
              "slength", "llength"]

identifier :: Parser Char -> Parser String
identifier first = guarded g (pure (:) <*> first <*> maybeSome gpChar)
  where g s = s `notElem` gpKeywords

lowerIdent :: Parser String
lowerIdent = identifier lower

upperIdent :: Parser String
upperIdent = identifier upper




ruleDecl :: Parser RuleDecl
ruleDecl = lowerIdent <| char '(' <*>  

factor :: Parser Factor
factor = lowerIdent <|> 

label :: Parser GP2Label
label = list <*> maybeOne (char '#' <| colour)




