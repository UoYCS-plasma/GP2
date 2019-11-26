module Parser.Program exposing (Condition(..), Decl(..), Expr(..), Program(..), TDecl(..), Var(..), VarType(..))

import Parser.Graph exposing (..)


type Program
    = Program (List TDecl)


type TDecl
    = Main Expr
    | Decl Decl


type Decl
    = Proc String (List Decl) Expr
    | Rule String (List Var) Graph Graph (Maybe Condition)


type Var
    = Var (List String) VarType


type VarType
    = ListT
    | AtomT
    | IntT
    | StringT
    | CharT


type Expr
    = If Expr Expr Expr
    | Try Expr Expr Expr
    | Loop Expr
    | Seq Expr Expr
    | RuleSet (List String)
    | ProcCall String
    | EOr Expr Expr
    | Skip
    | Fail
    | Break


type Condition
    = And Condition Condition
    | Or Condition Condition
    | Not Condition
    | LT IntExpr IntExpr
    | LE IntExpr IntExpr
    | GT IntExpr IntExpr
    | GE IntExpr IntExpr
    | EQ ListExpr ListExpr
    | NE ListExpr ListExpr
    | EdgePred String String (Maybe Label)
    | VarType VarType String
