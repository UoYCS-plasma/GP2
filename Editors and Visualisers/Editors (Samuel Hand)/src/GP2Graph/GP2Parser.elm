module GP2Graph.GP2Parser exposing (parse, position, whitespace, lexeme, stmtList)


import Parser exposing (..)
import DotLang exposing (Dot(..), Stmt(..), ID(..), NodeId(..), AttrStmtType(..), EdgeRHS(..), EdgeType(..), Attr(..))
import GP2Graph.RuleListParser exposing (id, listExpr)


parse : String -> Result (List DeadEnd) Dot
parse graph =
    run gp2GraphParser graph


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')


lexeme : Parser a -> Parser a
lexeme parser =
    parser |. whitespace


negFloat : Parser Float
negFloat =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= float
        , float
        ]


gp2GraphParser : Parser Dot
gp2GraphParser =
    succeed (Dot Digraph Nothing)
        |. whitespace
        |. (symbol >> lexeme) "["
        |. oneOf [ succeed () |. position |. (symbol >> lexeme) "|" , succeed () ]
        |= stmtList
        |. (symbol >> lexeme) "]"


position : Parser String
position =
    succeed (\a b -> a++","++b)
        |. (symbol >> lexeme) "<"
        |= (succeed String.fromFloat |= lexeme negFloat)
        |. (symbol >> lexeme) ","
        |= (succeed String.fromFloat |= lexeme negFloat)
        |. (symbol >> lexeme) ">"


stmtList : Parser (List Stmt)
stmtList =
    (loop [] (item node) |. (symbol >> lexeme) "|") |> andThen (\graph -> loop graph (item edge))


item : Parser Stmt -> (List Stmt) -> Parser (Step (List Stmt) (List Stmt))
item parser list =
    oneOf [ succeed (\parsed -> Loop (parsed::list)) |= parser, succeed (Done list) ]


nodeId : Parser NodeId
nodeId =
    succeed (\id -> NodeId (ID id) Nothing)
        |= lexeme id


edgeId : Parser Attr
edgeId =
    succeed (Attr (ID "id"))
        |= (succeed ID |= lexeme id)


parseList : List (Parser a) -> Parser (List a)
parseList list =
    case list of
        (x::xs) ->
            andThen (\a -> map ((::) a) (parseList xs)) x

        _ ->
            succeed []


hostList : Parser String
hostList =
    succeed String.slice
        |= getOffset
        |. listExpr
        |= getOffset
        |= getSource
        |> andThen (\s -> if s == "empty" then succeed "" else succeed s)


root : Parser (Maybe Attr)
root =
    succeed (Attr (ID "style") >> Just)
        |= (succeed ID |= oneOf [ succeed "filled,bold" |. (symbol >> lexeme) "(R)", succeed "filled,solid" ])
        |. (symbol >> lexeme) ","


bidirectional : Parser Attr
bidirectional =
    succeed (ID >> Attr (ID "dir"))
        |= oneOf
            [ succeed "both" |. (symbol >> lexeme) "(B)"
            , succeed "forward"
            ]


nodeColour : Parser (Maybe Attr)
nodeColour =
    oneOf
        [ succeed (Attr (ID "fillcolor") >> Just)
            |= (succeed ID
                |. (symbol >> lexeme) "#"
                |= oneOf
                    [ succeed "white" |. (symbol >> lexeme) "none"
                    , succeed "grey" |. (symbol >> lexeme) "grey"
                    , succeed "red" |. (symbol >> lexeme) "red"
                    , succeed "green" |. (symbol >> lexeme) "green"
                    , succeed "blue" |. (symbol >> lexeme) "blue"
                    , succeed "pink" |. (symbol >> lexeme) "any"
                    ]
            )
        , succeed (Just (Attr (ID "fillcolor") (ID "white")))
        ]


edgeColour : Parser (Maybe Attr)
edgeColour =
        oneOf [
        succeed identity
        |. (symbol >> lexeme) "#"
        |= oneOf
            [ succeed (Attr (ID "color") >> Just)
                |= (succeed ID
                    |= oneOf
                        [ succeed "black" |. (symbol >> lexeme) "none"
                        , succeed "red" |. (symbol >> lexeme) "red"
                        , succeed "green" |. (symbol >> lexeme) "green"
                        , succeed "blue" |. (symbol >> lexeme) "blue"
                        , succeed "pink" |. (symbol >> lexeme) "any"
                        ]
                )
            , succeed identity |. (symbol >> lexeme) "dashed" |= succeed (Just (Attr (ID "style") (ID "dashed"))) ]
            , succeed (Just (Attr (ID "color") (ID "black")))
            ]



pos : Parser (Maybe Attr)
pos =
    oneOf
        [ succeed (Attr (ID "pos") >> Just)
            |= (succeed ID |= position)
        , succeed Nothing
        ]


label : Parser (Maybe Attr)
label =
    succeed (Attr (ID "label") >> Just)
        |= (succeed ID |= hostList)


node : Parser Stmt
node =
    succeed NodeStmt
        |. (symbol >> lexeme) "("
        |= nodeId
        |= ((parseList [ root, label, nodeColour, pos ]) |> map (List.filterMap identity))
        |. (symbol >> lexeme) ")"


edge : Parser Stmt
edge =
    succeed (\id bidir from to attrs -> EdgeStmtNode from (EdgeNode to) [] (id::bidir::attrs))
        |. (symbol >> lexeme) "("
        |= edgeId
        |= bidirectional
        |. (symbol >> lexeme) ","
        |= nodeId
        |. (symbol >> lexeme) ","
        |= nodeId
        |. (symbol >> lexeme) ","
        |= (parseList [ label, edgeColour ] |> map (List.filterMap identity))
        |. (symbol >> lexeme) ")"
