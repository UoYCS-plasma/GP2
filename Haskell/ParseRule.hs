import ParseLib
import ParseGraph


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

type Variable = String
type NodeId = String

data GP2Label = GP2Label GPList Colour 
type GPList = [Atom]

-- Not sure what the standard practice is for implementing arithmetic 
-- operator precedence in this framework.
data Atom = | Var Id 
            | Val Constant
	    | Indeg NodeId
	    | Outdeg NodeId
            | Llength GPList
            | Slength GPList
            | Neg Atom
	    | Plus Atom Atom
	    | Minus Atom Atom
	    | Times Atom Atom
	    | Div Atom Atom
	    | Concat Atom Atom
            		

atom :: Parser Atom
atom = pure Var <*> lowerIdent
   <|> pure Val <*> value
   -- if this does not typecheck, try replacing char 'x' with keyword "x"
   <|> keyword "indeg" |> char '(' |> pure Indeg <*> lowerIdent <| char ')'
   <|> keyword "outdeg" |> char '(' |> pure Indeg <*> lowerIdent <| char ')'
   <|> keyword "llength" |> char '(' |> pure Llength <*> 

	 

intConst :: Parser Int
intConst = pure read <*> atLeastOne numChar <| optSpaces
	 

gp2Label :: Parser GP2Label
gp2Label = pure GP2Label <*> maybeSome nodeValue <*> nodeColour




