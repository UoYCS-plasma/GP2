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


data Colour = Uncoloured
            | Red 
            | Green 
            | Blue
            | Grey 
            | Dashed 
            | Cyan deriving (Eq, Show)

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


data VarType = IntVar
             | ChrVar
             | StrVar
             | AtomVar
             | ListVar
   deriving (Eq, Show)

gpTypes :: [ (String, VarType) ]
gpTypes = [
    ("int", IntVar),
    ("char", ChrVar),
    ("string", StrVar),
    ("atom", AtomVar),
    ("list", ListVar) ]

type ID = String


-- GP Program ADTs
data GPProgram = Program [Declaration] deriving (Show)

data Declaration = MainDecl Main
                 | ProcDecl Procedure
                 | RuleDecl Rule
     deriving (Show)

data Main = Main CommandSequence deriving (Show)

data Procedure = Procedure ID [Declaration] CommandSequence deriving (Show)

data CommandSequence = Sequence [Command] deriving (Show) 

data Command = Block Block
             | IfStatement Block Block Block 
             | TryStatement Block Block Block
    deriving (Show)


data Block = ComSeq CommandSequence
           | LoopedComSeq CommandSequence
           | SimpleCommand SimpleCommand
           | ProgramOr Block Block      
    deriving (Show)
      

data SimpleCommand = RuleCall ID
                   | LoopedRuleCall ID
                   | RuleSetCall [ID]
                   | LoopedRuleSetCall [ID] 
                   | ProcedureCall ID
                   | LoopedProcedureCall ID
                   | Skip
                   | Fail
    deriving (Show)



-- GP Rule ADTs
type Variable = (ID, VarType)
type Interface = [ID]
type Source = ID
type Target = ID

data Rule = Rule ID [Variable] (AstRuleGraph, AstRuleGraph) Interface Condition String
    deriving (Show)

-- Rule graph labels are lists of expressions.
type RuleGraph = Graph RuleNode RuleLabel
data AstRuleGraph = AstRuleGraph [RuleNode] [RuleEdge] deriving (Show)
data RuleNode = RuleNode ID Bool RuleLabel deriving (Show)
data RuleEdge = RuleEdge Bool Source Target RuleLabel deriving (Show)

type GPList = [RuleAtom]
data RuleLabel = RuleLabel GPList Colour  deriving (Show)

data RuleAtom = Var Variable
              | Val HostAtom
              | Indeg ID
              | Outdeg ID
              -- RHS only
              | Llength GPList
              | Slength RuleAtom
              | Neg RuleAtom
              | Plus RuleAtom RuleAtom
              | Minus RuleAtom RuleAtom
              | Times RuleAtom RuleAtom
              | Div RuleAtom RuleAtom
              | Concat RuleAtom RuleAtom
    deriving (Show)


instance Ord VarType where
    ListVar < vt = False
    AtomVar < vt = vt == ListVar
    IntVar  < vt = vt `elem` [ListVar, AtomVar]
    StrVar  < vt = vt `elem` [ListVar, AtomVar]
    ChrVar  < vt = vt `elem` [ListVar, AtomVar, StrVar] 

-- TODO: precedence of infix binary operators
-- Is it possible to do BinOp Atom Atom and
-- data BinOp = Plus | Min | ... ?

data Condition = NoCondition
               | TestInt ID
               | TestChr ID
               | TestStr ID
               | TestAtom ID
               | Edge ID ID (Maybe RuleLabel)
               | Eq GPList GPList
               | NEq GPList GPList
               | Greater RuleAtom RuleAtom
               | GreaterEq RuleAtom RuleAtom
               | Less RuleAtom RuleAtom
               | LessEq RuleAtom RuleAtom
               | Not Condition
               | Or Condition Condition
               | And Condition Condition
    deriving (Show)


data HostNode = HostNode ID Bool HostLabel deriving (Show)
data HostEdge = HostEdge Source Target HostLabel deriving (Show)

-- Host Graph ADTs
type HostGraph = Graph HostNode HostLabel
data AstHostGraph = AstHostGraph [HostNode] [HostEdge] deriving (Show)
data HostLabel = HostLabel [HostAtom] Colour deriving (Eq, Show)
data HostAtom = Int Int
              | Str String 
              | Chr Char deriving (Eq, Show)



 



