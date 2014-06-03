module GPSyntax where

import Graph

gpNumChars, gpChars :: [Char]
gpNumChars = ['0'..'9']
gpChars = concat [ ['A'..'Z'] , ['a'..'z'] , gpNumChars , ['_'] ]

{- Colours have yet to be formalised. Currently working on the assumption that only one can be applied to a given edge or node -}
data Colour   = Uncoloured | Red | Green | Blue | Grey | Cyan | Dashed deriving (Eq, Show)

gpHostColours :: [ (String, Colour) ]
gpHostColours = [
    ("uncoloured", Uncoloured),
    ("red", Red),
    ("green", Green),
    ("blue", Blue), 
    ("grey", Grey),
--  ("cyan", Cyan),
    ("dashed", Dashed) ]

gpColours :: [ (String, Colour) ]
gpColours = ("cyan", Cyan) : gpHostColours

gpKeywords :: [String]
gpKeywords = map fst gpColours ++
             ["main", "if", "try", "then", "else", "or", "skip", 
	      "fail", "int", "char", "string", "atom", "list",
              "interface", "where", "injective", "true", "false",
              "and", "not", "edge", "empty", "indeg", "outdeg",
              "slength", "llength"]

data GPNode = GPHostNode Boolean String GPHostLabel
            | GPRuleNode Boolean String GPLabel

-- Host graph labels are lists of constants.
type GPHostGraph = Graph GPHostLabel
data GPHostLabel = GPHostLabel GPHostList Colour HostNodeId Boolean deriving (Eq, Show)
type GPHostList = [HostAtom]
data HostAtom = Int Int
	      | Str String 
	      | Chr Char deriving (Eq, Show)

-- Rule graph labels are lists of expressions.
type GPRuleGraph = ( [Condition], Graph GPLabel)
data GPLabel = GPLabel GPList Colour 
type GPList = [Atom]

type Variable = String
type HostNodeId = String
type RuleNodeId = String

-- TODO: precedence of infix binary operators
-- Is it possible to do BinOp Atom Atom and
-- data BinOp = Plus | Min | ... ?
data Atom = Var Variable 
          | Val HostAtom
	  | Indeg RuleNodeId
	  | Outdeg RuleNodeId
          | Llength GPList
          | Slength GPList
          | Neg Atom
	  | Plus Atom Atom
	  | Minus Atom Atom
	  | Times Atom Atom
	  | Div Atom Atom
	  | Concat Atom Atom

data Condition = TestInt Variable
               | TestChar Variable
               | TestStr Variable
               | TestAtom Variable
	       | Edge RuleNodeId RuleNodeId GPLabel
               | Eq GPList GPList
               | NEq GPList GPList
               | Greater Atom Atom
               | GreaterEq Atom Atom
               | Less Atom Atom
               | LessEq Atom Atom
               | Not Condition
               | Or Condition Condition
               | And Condition Condition

 



