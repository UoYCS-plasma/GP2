module GPSyntax where

import Graph

gpNumChars, gpChars :: [Char]
gpNumChars = ['0'..'9']
gpChars = concat [ ['A'..'Z'] , ['a'..'z'] , gpNumChars , ['_'] ]

keywords :: [String]
keywords = map fst hostColours ++
           ["main", "if", "try", "then", "else", "or", "skip", 
            "fail", "int", "char", "string", "atom", "list",
            "interface", "where", "injective", "true", "false",
            "and", "not", "edge", "empty", "indeg", "outdeg",
            "slength", "llength"]


{- Colours have yet to be formalised. Currently working on the assumption that only one can be applied to a given edge or node -}
data Colour = Uncoloured | Red | Green | Blue | Grey | Cyan | Dashed deriving (Eq, Show)

hostColours :: [ (String, Colour) ]
hostColours = [
    ("uncoloured", Uncoloured),
    ("red", Red),
    ("green", Green),
    ("blue", Blue), 
    ("grey", Grey),
    ("dashed", Dashed) ]

ruleColours :: [ (String, Colour) ]
ruleColours = ("cyan", Cyan) : hostColours



-- GP Program ADTs
data GPProgram = Program [Declaration]

data Declaration = MainDecl Main
                 | ProcDecl Procedure
                 | RuleDecl Rule

data Main = Main CommandSequence

data Procedure = Procedure String [LocalDecl] CommandSequence

data LocalDecl = LocalRule Rule
               | LocalProcedure Procedure

data CommandSequence = ComSeq [Command]

data Command = Block Block
             | IfThen Block Block
             | IfThenElse Block Block Block
             | Try Block
             | TryThen Block Block
             | TryElse Block Block
             | TryThenElse Block Block Block

data Block = LoopedComSeq CommandSequence
           | SimpleCommand SimpleCommand
           | ProgramOr Block Block             

data SimpleCommand = RuleSetCall [String]
                   | LoopedRuleSetCall [String] 
                   | ProcedureCall String
                   | LoopedProcedureCall String
                   | SkipStatement


                   | FailStatement






-- GP Rule ADTs
type Variables = ([Variable], String)
type Interface = [ID]
type ID = String
type Source = String
type Target = String

data Rule = Rule ID [Variables] (RuleGraph, RuleGraph) Interface Condition String

-- Rule graph labels are lists of expressions.
data RuleGraph = RuleGraph [RuleNode] [RuleEdge]
data RuleNode = RuleNode ID String RuleLabel
data RuleEdge = RuleEdge Source Target RuleLabel

type GPList = [RuleAtom]
data RuleLabel = RuleLabel GPList Colour 

data RuleAtom = Var Variable 
              | Val HostAtom
              | Indeg ID
              | Outdeg ID
              | Llength GPList
              | Slength GPList
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

data Condition = Nothing
               | TestInt Variable
               | TestChar Variable
               | TestStr Variable
               | TestAtom Variable
               | Edge ID ID RuleLabel
               | Eq GPList GPList
               | NEq GPList GPList
               | Greater RuleAtom RuleAtom
               | GreaterEq RuleAtom RuleAtom
               | Less RuleAtom RuleAtom
               | LessEq RuleAtom RuleAtom
               | Not Condition
               | Or Condition Condition
               | And Condition Condition


data HostNode = HostNode ID String HostLabel
data HostEdge = HostEdge Source Target HostLabel

-- Host Graph ADTs
data HostGraph = HostGraph [HostNode] [HostEdge]
data HostLabel = HostLabel [HostAtom] Colour deriving (Eq, Show)
data HostAtom = Int Int
              | Str String 
              | Chr Char deriving (Eq, Show)



 



