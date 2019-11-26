module GP2Graph.GraphParser exposing (parse)

import Parser exposing (..)
import Graph
import GP2Graph.GP2Graph as GP2Graph
import GP2Graph.HostListParser exposing (hostListParser)
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import Geometry.Vec2 as Vec2 exposing (Vec2)


type alias SingleEdge =
    { id : String
    , flag : Bool
    , from : String
    , to : String
    , label : String
    , mark : GP2Graph.Mark
    }


parse : String -> Result (List DeadEnd) GP2Graph.VisualGraph
parse graph =
    run (hostGraphParser |. end) graph


hostGraphParser : Parser GP2Graph.VisualGraph
hostGraphParser =
    succeed identity
        |. (symbol >> lexeme) "["
        |= (loop Graph.empty nodes |> andThen nodesAndEdges)
        |. (symbol >> lexeme) "]"


nodesAndEdges : GP2Graph.VisualGraph -> Parser GP2Graph.VisualGraph
nodesAndEdges graph =
    succeed identity
        |. (symbol >> lexeme) "|"
        |= loop graph edges


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')


notWhitespace : Parser ()
notWhitespace =
    chompWhile (\c -> c /= ' ' || c /= '\n' || c /= '\r' || c /= '\t')


lexeme : Parser a -> Parser a
lexeme parser =
    parser |. whitespace


edges : GP2Graph.VisualGraph -> Parser (Step GP2Graph.VisualGraph GP2Graph.VisualGraph)
edges graph =
    oneOf [ edge |> andThen (makeEdge graph) |> map Loop, succeed (Done graph) ]


makeEdge : GP2Graph.VisualGraph -> SingleEdge -> Parser GP2Graph.VisualGraph
makeEdge graph { id, flag, from, to, label, mark } =
    Maybe.map2
        (\n1 n2 -> GP2Graph.createEdge { id = id, flag = flag, label = label, mark = mark } n1 n2 graph)
        (GP2Graph.internalId from graph)
        (GP2Graph.internalId to graph)
        |> Maybe.map succeed
        |> Maybe.withDefault (problem "Edge references non-existant node id")


nodes : GP2Graph.VisualGraph -> Parser (Step GP2Graph.VisualGraph GP2Graph.VisualGraph)
nodes graph =
    oneOf [ succeed (\n -> Loop (GP2Graph.createNode n graph)) |= node, succeed (Done graph) ]


node : Parser (GP2Graph.Label, Ellipse)
node =
    succeed Tuple.pair
        |. (symbol >> lexeme) "("
        |= nodeLabel
        |= ellipse
        |. (symbol >> lexeme) ")"


ellipse : Parser Ellipse
ellipse =
    succeed Ellipse
        |= center
        |= succeed 25
        |= succeed 25


negFloat : Parser Float
negFloat =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= float
        , float
        ]


center : Parser Vec2
center =
    succeed Vec2
        |. (symbol >> lexeme) "<"
        |= lexeme negFloat
        |. (symbol >> lexeme) ","
        |= lexeme negFloat
        |. (symbol >> lexeme) ">"


edge : Parser SingleEdge
edge =
    succeed SingleEdge
        |. (symbol >> lexeme) "("
        |= (lexeme int |> map String.fromInt)
        |= lexeme (oneOf [ succeed True |. symbol "(B)", succeed False ])
        |. (symbol >> lexeme) ","
        |= (lexeme int |> map String.fromInt)
        |. (symbol >> lexeme) ","
        |= (lexeme int |> map String.fromInt)
        |. (symbol >> lexeme) ","
        |= hostList
        |= lexeme (oneOf [ succeed identity |. (symbol >> lexeme) "#" |= gpMark, succeed GP2Graph.None ])
        |. (symbol >> lexeme) ")"


nodeLabel : Parser GP2Graph.Label
nodeLabel =
    succeed GP2Graph.Label
        |= (lexeme int |> map String.fromInt)
        |= lexeme (oneOf [ succeed True |. symbol "(R)", succeed False ])
        |. (symbol >> lexeme) ","
        |= hostList
        |= lexeme (oneOf [ succeed identity |. (symbol >> lexeme) "#" |= gpMark, succeed GP2Graph.None ])


hostList : Parser String
hostList =
    succeed String.slice
        |= getOffset
        |. hostListParser
        |= getOffset
        |= getSource
        |> andThen (\s -> if s == "empty" then succeed "" else succeed s)


gpMark : Parser GP2Graph.Mark
gpMark =
    oneOf
        [ succeed GP2Graph.None |. keyword "none"
        , succeed GP2Graph.Any |. keyword "any"
        , succeed GP2Graph.Grey |. keyword "grey"
        , succeed GP2Graph.Dashed |. keyword "dashed"
        , succeed GP2Graph.Red |. keyword "red"
        , succeed GP2Graph.Blue |. keyword "blue"
        , succeed GP2Graph.Green |. keyword "green"
        ]
