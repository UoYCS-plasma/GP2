module Parser.Interpreter exposing (run)

import Debug
import Parser.GP2Parser exposing (parseGraph, parseProgram)
import Basics.Extra exposing (flip)
import Parser.Graph exposing (Graph(..))
import Dict exposing (Dict)
import Parser.Compiler exposing(..)
import Result.Extra
import Maybe.Extra
import Parser.Program exposing(Program(..), Expr(..))


run : String -> String -> String
run prog graph =
    case (Result.andThen (flip compile (CProgram { procs = Dict.empty, rules = Dict.empty } Nothing)) (parseProgram prog), parseGraph graph) of
        (Ok (CProgram d expr), Ok g) -> case expr of
            Nothing -> "No Main procedure found"
            Just e -> Result.Extra.merge <| Result.map (Maybe.Extra.unwrap "Program produced failure" Graph.toString) (eval d e g)
        (Err s, Ok _) -> "Errors parsing program: "++s
        (Ok _, Err s) -> "Errors parsing graph: "++s
        (Err s1, Err s2) -> "Errors parsing program: "++s1++"\nErrors parsing graph: "++s2


callProc : DeclTable -> Graph -> ProcBody -> Result String (Maybe Graph)
callProc d1 g (ProcBody d2 e) = eval { procs = Dict.union d2.procs d1.procs, rules = Dict.union d2.rules d1.rules } e g

applyRule : Graph -> RuleBody -> Result String (Maybe Graph)
applyRule g r = Ok (Just g)


eval : DeclTable -> Expr -> Graph -> Result String (Maybe Graph)
eval decls expr graph =
    case expr of
        Seq e1 e2 -> eval decls e1 graph |> Result.andThen (Maybe.Extra.unwrap (Ok Nothing) (eval decls e2))
        EOr e1 e2 -> eval decls e1 graph
        Skip -> Ok (Just graph)
        Fail -> Ok (Nothing)
        RuleSet (x::xs) -> (Result.map2 Maybe.Extra.or) (Result.fromMaybe ("Rule not declared: "++x) (Dict.get x decls.rules) |> Result.andThen (applyRule graph)) (eval decls (RuleSet xs) graph)
        RuleSet [] -> Ok (Nothing)
        ProcCall p -> Result.fromMaybe ("Procedure not declared: "++p) (Dict.get p decls.procs) |> Result.andThen (callProc decls graph)
        Loop e -> eval decls e graph |> Result.andThen (Maybe.Extra.unwrap (Ok (Just graph)) (eval decls (Loop e)))
        If e1 e2 e3 -> eval decls e1 graph |> Result.andThen (Maybe.Extra.unwrap (eval decls e3 graph) (always <| eval decls e2 graph))
        Try e1 e2 e3 -> eval decls e1 graph |> Result.andThen (Maybe.Extra.unwrap (eval decls e3 graph) (eval decls e2))
        Break -> Ok (Just graph)
