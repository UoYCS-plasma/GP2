module GP2Graph.GP2Rule exposing (GP2Rule, toGP2, toDot, fromDot)


import GP2Graph.GP2Graph as GP2Graph
import Graph exposing (Graph)
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import DotLang exposing (Attr(..), Dot(..), EdgeRHS(..), EdgeType(..), ID(..), Stmt(..), AttrStmtType(..))


type alias GP2Rule =
    { vars : String
    , left : GP2Graph.VisualGraph
    , right : GP2Graph.VisualGraph
    , condition : String
    }


toGP2 : GP2Rule -> String
toGP2 { vars, left, right, condition } =
    vars
        ++ "\n"
        ++ (GP2Graph.toGP2 left)
        ++ "\n=>\n"
        ++ (GP2Graph.toGP2 right)
        ++ "\ninterface = \n{\n"
        ++ ((GP2Graph.interface left right)
            |> String.join ", ")
        ++ "\n}"
        ++ (if condition /= "" then "\nwhere " ++ condition else "")


nodeToDot : String -> String -> String -> String -> String -> Ellipse -> Stmt
nodeToDot shape id label colour style { center } =
    NodeStmt
        (DotLang.NodeId (ID id) Nothing)
        [ Attr (ID "pos") (ID (String.fromFloat center.x ++ "," ++ String.fromFloat center.y ++ "!"))
        , Attr (ID "label") (ID label)
        , Attr (ID "fillcolor") (ID colour)
        , Attr (ID "style") (ID style)
        , Attr (ID "shape") (ID shape)
        ]


dotEdge : String -> String -> String -> String -> String -> String -> String -> String -> Stmt
dotEdge shape id label colour direction style from to =
    EdgeStmtNode
        (DotLang.NodeId (ID from) Nothing)
        (EdgeNode (DotLang.NodeId (ID to) Nothing))
        []
        [ Attr (ID "label") (ID label)
        , Attr (ID "dir") (ID direction)
        , Attr (ID "color") (ID colour)
        , Attr (ID "style") (ID style)
        , Attr (ID "id") (ID id)
        , Attr (ID "arrowhead") (ID shape)
        , Attr (ID "arrowtail") (ID shape)
        ]


leftNodeToDot : GP2Graph.VisualGraph -> Graph.Node ( GP2Graph.Label, Ellipse ) -> Stmt
leftNodeToDot right node =
    case GP2Graph.getCorrespondingNode node right of
        Just { label } ->
            nodeToDot
                "ellipse"
                (Tuple.first node.label).id
                ((Tuple.first node.label).label++"/"++(Tuple.first label).label)
                ((GP2Graph.markToColour (Tuple.first node.label).mark |> Tuple.second)++";0.5:"++(GP2Graph.markToColour (Tuple.first label).mark |> Tuple.second))
                ("filled,"++(if (Tuple.first node.label).flag then "bold," else "solid,")++(if (Tuple.first label).flag then "bold" else "solid"))
                (Tuple.second node.label)

        Nothing ->
            nodeToDot
                "rect"
                (Tuple.first node.label).id
                (Tuple.first node.label).label
                (GP2Graph.markToColour (Tuple.first node.label).mark |> Tuple.second)
                ("filled,"++(if (Tuple.first node.label).flag then "bold" else "solid"))
                (Tuple.second node.label)


rightNodeToDot : GP2Graph.VisualGraph -> Graph.Node ( GP2Graph.Label, Ellipse ) -> Maybe Stmt
rightNodeToDot left node =
    case GP2Graph.getCorrespondingNode node left of
        Just _ ->
            Nothing

        Nothing ->
            nodeToDot
                "diamond"
                (Tuple.first node.label).id
                (Tuple.first node.label).label
                (GP2Graph.markToColour (Tuple.first node.label).mark |> Tuple.second)
                ("filled,"++(if (Tuple.first node.label).flag then "bold" else "solid"))
                (Tuple.second node.label)
                |> Just


edgeToDot : String -> String -> GP2Graph.VisualGraph -> String -> String -> GP2Graph.Label -> Stmt
edgeToDot side shape other from to edge =
    case GP2Graph.getCorrespondingEdge edge other of
        Just _ ->
            dotEdge
                shape
                (side++"_"++edge.id)
                edge.label
                (GP2Graph.markToColour edge.mark |> Tuple.first)
                (if edge.flag then "both" else "forward")
                (if edge.mark == GP2Graph.Dashed then "dashed" else "solid")
                from
                to

        Nothing ->
            dotEdge
                shape
                edge.id
                edge.label
                (GP2Graph.markToColour edge.mark |> Tuple.first)
                (if edge.flag then "both" else "forward")
                (if edge.mark == GP2Graph.Dashed then "dashed" else "solid")
                from
                to


edgesToDot : String -> String -> GP2Graph.VisualGraph -> GP2Graph.VisualGraph -> Graph.Edge (List GP2Graph.Label) -> Maybe (List Stmt)
edgesToDot side shape graph other edge =
    let
        from =
            Graph.get edge.from graph
                |> Maybe.map (.node >> .label >> Tuple.first >> .id)

        to =
            Graph.get edge.to graph
                |> Maybe.map (.node >> .label >> Tuple.first >> .id)
    in
    Maybe.map2
        ( \f t -> List.map (edgeToDot side shape other f t) edge.label) from to




toDot : GP2Rule -> String
toDot { vars, left, right, condition } =
    Dot
        DotLang.Digraph
        (Just (ID (vars++"="++condition)))
        (List.map (leftNodeToDot right) (Graph.nodes left) ++ List.filterMap (rightNodeToDot left) (Graph.nodes right)
        ++ (List.concat <| List.filterMap (edgesToDot "left" "obox" left right) (Graph.edges left)) ++ (List.concat <| List.filterMap (edgesToDot "right" "odiamond" right left) (Graph.edges right))
        )
        |> DotLang.toString


getData : ID -> Maybe (String, String)
getData id =
    case id of
        ID str ->
            let
                words =
                    String.split "=" str
            in
            Maybe.map2
                Tuple.pair
                (List.head words)
                (List.tail words |> Maybe.map (String.join "="))

        _ ->
            Nothing


fromDot : Dot -> GP2Rule
fromDot (Dot _ data stmts) =
    { vars = Maybe.andThen getData data
        |> Maybe.map Tuple.first
        |> Maybe.withDefault ""
    , condition = Maybe.andThen getData data
        |> Maybe.map Tuple.second
        |> Maybe.withDefault ""
    , left =
        List.foldl (nodeFromDot "rect" Tuple.first) Graph.empty stmts
            |> (\graph -> List.foldl (edgeFromDot "obox" "left") graph stmts)
    , right =
        List.foldl (nodeFromDot "diamond" Tuple.second) Graph.empty stmts
            |> (\graph -> List.foldl (edgeFromDot "odiamond" "right") graph stmts)
    }


removePrefix : String -> List Attr -> List Attr
removePrefix prefix attrs =
    List.map
        (\attr ->
            case attr of
                Attr (ID "id") (ID id) ->
                    if String.startsWith (prefix++"_") id then
                        Attr (ID "id") (ID (String.dropLeft ((String.length prefix)+1) id))

                    else
                        attr
                _ ->
                    attr
        )
        attrs


edgeFromDot : String -> String -> Stmt -> GP2Graph.VisualGraph -> GP2Graph.VisualGraph
edgeFromDot shape side stmt graph =
    case stmt of
        EdgeStmtNode (DotLang.NodeId from _) (EdgeNode (DotLang.NodeId to _)) _ attrs ->
            case GP2Graph.getAttr "arrowhead" attrs of
                Just str ->
                    if str == shape then
                        GP2Graph.edgeFromDot (EdgeStmtNode (DotLang.NodeId from Nothing) (EdgeNode (DotLang.NodeId to Nothing)) [] (removePrefix side attrs)) graph

                    else
                        graph

                _ ->
                    graph

        _ ->
            graph


nodeFromDot : String -> ( (String, String) -> String) -> Stmt -> GP2Graph.VisualGraph -> GP2Graph.VisualGraph
nodeFromDot shape unjoin stmt graph =
    case stmt of
        NodeStmt (DotLang.NodeId id _) attrs ->
            case GP2Graph.getAttr "shape" attrs of
                Just "ellipse" ->
                    Maybe.map5
                        (\pos i label mark flag -> GP2Graph.createNode ({ id = i, label = label, mark = mark, flag = flag }, pos) graph)
                        (GP2Graph.getDotPos attrs)
                        (GP2Graph.getDotId id)
                        (getDotLabel unjoin attrs)
                        (getDotMark unjoin attrs)
                        (getDotRoot unjoin attrs)
                        |> Maybe.withDefault graph

                Just str ->
                    if str == shape then
                        GP2Graph.nodeFromDot stmt graph

                    else
                        graph

                _ ->
                    graph

        _ ->
            graph


nodeColourToMark : String -> GP2Graph.Mark
nodeColourToMark colour =
    case colour of
        "pink" ->
            GP2Graph.Any

        "grey" ->
            GP2Graph.Grey

        "red" ->
            GP2Graph.Red

        "green" ->
            GP2Graph.Green

        "blue" ->
            GP2Graph.Blue

        _ ->
            GP2Graph.None


getDotLabel : ( (String, String) -> String) -> List Attr -> Maybe String
getDotLabel unjoin attrs =
    let
        words =
            GP2Graph.getAttr "label" attrs
                |> Maybe.map (String.split "/")
    in
    Maybe.map2
        Tuple.pair
        (Maybe.andThen List.head words)
        (Maybe.andThen List.tail words |> Maybe.map (String.join "/"))
        |> Maybe.map unjoin


getDotMark : ( (String, String) -> String) -> List Attr -> Maybe GP2Graph.Mark
getDotMark unjoin attrs =
    let
        words =
            GP2Graph.getAttr "fillcolor" attrs
                |> Maybe.map (String.split ":")
    in
    Maybe.map2
        Tuple.pair
        (Maybe.andThen List.head words |> Maybe.map (String.split ";") |> Maybe.andThen List.head)
        (Maybe.andThen List.tail words |> Maybe.map (String.join ":"))
        |> Maybe.map unjoin
        |> Maybe.map nodeColourToMark


getDotRoot : ( (String, String) -> String) -> List Attr -> Maybe Bool
getDotRoot unjoin attrs =
    let
        words =
            GP2Graph.getAttr "style" attrs
                |> Maybe.map (String.split ",")

    in
    Maybe.map2
        Tuple.pair
        (Maybe.andThen List.tail words |> Maybe.andThen List.head)
        (Maybe.andThen List.tail words |> Maybe.andThen List.tail |> Maybe.map (String.join ","))
        |> Maybe.map unjoin
        |> Maybe.map (\s -> String.contains s "bold")
