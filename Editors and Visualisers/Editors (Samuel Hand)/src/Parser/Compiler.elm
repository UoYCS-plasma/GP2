module Parser.Compiler exposing (DeclTable, VarTable, CProgram(..), ProcBody(..), RuleBody(..), compile)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Parser.Graph exposing (Graph)
import List.Extra exposing (allDifferent)
import Parser.Program exposing (..)


type alias DeclTable =
    { procs : Dict String ProcBody, rules : Dict String RuleBody }


type alias VarTable =
    Dict String VarType


type CProgram
    = CProgram DeclTable (Maybe Expr)


type ProcBody
    = ProcBody DeclTable Expr


type RuleBody
    = RuleBody VarTable Graph Graph (Maybe Condition)


compile : Program.Program -> CProgram -> Result String CProgram
compile (Program.Program decls) current =
    List.foldl (Result.andThen << compileTDecl) (Ok current) decls


compileTDecl : TDecl -> CProgram -> Result String CProgram
compileTDecl tdecl (CProgram table mainExpr) =
    case tdecl of
        Main expr ->
            if mainExpr == Nothing then
                Ok (CProgram table (Just expr))

            else
                Err "Saw two declarations for Main"

        Decl decl ->
            Result.map (flip CProgram mainExpr) (compileDecl decl table)


compileDecl : Decl -> DeclTable -> Result String DeclTable
compileDecl decl ({ procs, rules } as table) =
    case decl of
        Proc name decls expr ->
            if Dict.member name procs then
                Err <| "Saw two declarations for procedure: "++name

            else
                let
                    localDecls =
                        List.foldl (Result.andThen << compileDecl) (Ok { procs = Dict.empty, rules = Dict.empty }) decls
                in
                Result.map (\a -> { table | procs = Dict.insert name (ProcBody a expr) procs }) localDecls

        Rule name vars g1 g2 cond ->
            if Dict.member name rules then
                Err <| "Saw two declarations for rule: "++name

            else
                let
                    varTable =
                        List.foldl (Result.andThen << compileVar) (Ok Dict.empty) vars
                in
                Result.map (\a -> { table | rules = Dict.insert name (RuleBody a g1 g2 cond) rules }) varTable


compileVar : Var -> VarTable -> Result String VarTable
compileVar (Var vars t) table =
    if not (allDifferent vars) || List.any (flip List.member (Dict.keys table)) vars then
        Err "Saw multiple declarations for a variable"

    else
        List.map (\a -> ( a, t )) vars |> Dict.fromList |> Dict.union table |> Ok
