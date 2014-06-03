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
    ("dashed", Dashed) ]

gpRuleColours :: [ (String, Colour) ]
gpRuleColours = ("cyan", Cyan) : gpHostColours

gpKeywords :: [String]
gpKeywords = map fst gpHostColours ++
             ["main", "if", "try", "then", "else", "or", "skip", 
              "fail", "int", "char", "string", "atom", "list",
              "interface", "where", "injective", "true", "false",
              "and", "not", "edge", "empty", "indeg", "outdeg",
              "slength", "llength"]

-- Identifier for nodes
type Id = String

-- Top-leve node type
data HostNode = HostNode Id Bool HostLabel
data RuleNode = RuleNode Id Bool RuleLabel

-- Host graph labels are lists of constants.
type HostGraph = Graph HostNode HostLabel
data HostLabel = HostLabel [HostAtom] Colour deriving (Eq, Show)
data HostAtom = Int Int
              | Str String 
              | Chr Char deriving (Eq, Show)

-- Rule graph labels are lists of expressions.
data RuleGraph = RuleGraph (Graph RuleNode RuleLabel) [Condition]
data RuleLabel = RuleLabel [RuleAtom] Colour 

data RuleAtom = Var Variable 
              | Val HostAtom
              | Indeg Id
              | Outdeg Id
              | Llength [RuleAtom]
              | Slength [RuleAtom]
              | Neg RuleAtom
              | Plus RuleAtom RuleAtom
              | Minus RuleAtom RuleAtom
              | Times RuleAtom RuleAtom
              | Div RuleAtom RuleAtom
              | Concat RuleAtom RuleAtom

type Variable = String

-- TODO: precedence of infix binary operators
-- Is it possible to do BinOp Atom Atom and
-- data BinOp = Plus | Min | ... ?

data Condition = TestInt Variable
               | TestChar Variable
               | TestStr Variable
               | TestAtom Variable
               | Edge Id Id RuleLabel
               | Eq [RuleAtom] [RuleAtom]
               | NEq [RuleAtom] [RuleAtom]
               | Greater RuleAtom RuleAtom
               | GreaterEq RuleAtom RuleAtom
               | Less RuleAtom RuleAtom
               | LessEq RuleAtom RuleAtom
               | Not Condition
               | Or Condition Condition
               | And Condition Condition

 



