module GP2Graph.GP2Graph exposing (HostList, HostListItem(..), RuleList, RuleListItem(..), StringExpr(..), IntExpr(..), Label, Mark(..), MultiContext, MultiGraph, VisualContext, VisualGraph, createEdge, createNode, deleteEdge, getEdgeData, getNodeData, internalId, isValid, nodePosition, setFlag, setId, setLabel, setMark, setNodeMajor, setNodeMinor, setNodePosition, toDot, toGP2, tryId, updateEdgeFlag, updateEdgeId, updateEdgeLabel, updateEdgeMark, updateNodeFlag, updateNodeId, updateNodeLabel, updateNodeMark, fromDot, interface, getCorrespondingNode, markToColour, getCorrespondingEdge, getAttr, getDotId, nodeFromDot, getDotPos, edgeFromDot)

import DotLang exposing (Attr(..), Dot(..), EdgeRHS(..), EdgeType(..), ID(..), Stmt(..))
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import Geometry.Vec2 exposing (Vec2)
import Graph exposing (Graph, Node, NodeContext, NodeId)
import IntDict
import Set
import List.Extra


type alias MultiGraph n e =
    Graph n (List e)


type alias MultiContext n e =
    NodeContext n (List e)


type alias VisualGraph =
    MultiGraph ( Label, Ellipse ) Label


type alias VisualContext =
    MultiContext ( Label, Ellipse ) Label


type Mark
    = None
    | Any
    | Grey
    | Dashed
    | Red
    | Blue
    | Green


type alias Label =
    { id : String
    , flag : Bool
    , label : String
    , mark : Mark
    }


type alias HostList =
    List HostListItem


type HostListItem
    = HostString String
    | HostInt Int
    | Empty


type alias RuleList =
    List RuleListItem


type RuleListItem
    = LVar String
    | IntExpr IntExpr
    | StringExpr StringExpr
    | RuleEmpty


type StringExpr
    = SVar String
    | SLiteral String
    | SCons StringExpr StringExpr


type IntExpr
    = IVar String
    | ILiteral Int
    | Add IntExpr IntExpr
    | Sub IntExpr IntExpr
    | Mul IntExpr IntExpr
    | Div IntExpr IntExpr
    | Neg IntExpr
    | Indeg String
    | Outdeg String
    | Length String


markToString : Mark -> String
markToString mark =
    case mark of
        None ->
            "none"

        Any ->
            "any"

        Grey ->
            "grey"

        Dashed ->
            "dashed"

        Red ->
            "red"

        Blue ->
            "blue"

        Green ->
            "green"


markToColour : Mark -> (String, String)
markToColour mark =
    case mark of
        None ->
            ("black", "white")

        Any ->
            ("pink", "pink")

        Grey ->
            ("grey", "grey")

        Dashed ->
            ("black", "black")

        Red ->
            ("red", "red")

        Blue ->
            ("blue", "blue")

        Green ->
            ("green", "green")


interface : VisualGraph -> VisualGraph -> List String
interface graph1 graph2 =
    Set.intersect
        ((Graph.nodes graph1)
            |> List.map (.label >> Tuple.first >> .id)
            |> Set.fromList)
        ((Graph.nodes graph2)
            |> List.map (.label >> Tuple.first >> .id)
            |> Set.fromList)
        |> Set.toList


nodesToGP2 : Graph.Node ( Label, Ellipse ) -> String -> String
nodesToGP2 node acc =
    let
        ( label, pos ) =
            node.label
    in
    acc
        ++ "\t("
        ++ label.id
        ++ (if label.flag then
                "(R)"

            else
                ""
           )
        ++ ", "
        ++ (if label.label == "" then "empty" else label.label)
        ++ (if label.mark /= None then
                "#" ++ markToString label.mark

            else
                ""
           )
        ++ "<"
        ++ String.fromFloat pos.center.x
        ++ ", "
        ++ String.fromFloat pos.center.y
        ++ ">"
        ++ ")\n"


edgeToGP2 : VisualContext -> VisualContext -> Label -> String -> String
edgeToGP2 from to label acc =
    acc
        ++ "\t("
        ++ label.id
        ++ (if label.flag then
                "(B)"

            else
                ""
           )
        ++ ", "
        ++ (Tuple.first from.node.label).id
        ++ ", "
        ++ (Tuple.first to.node.label).id
        ++ ", "
        ++ (if label.label == "" then "empty" else label.label)
        ++ (if label.mark /= None then
                "#" ++ markToString label.mark

            else
                ""
           )
        ++ ")\n"


edgesToGP2 : VisualGraph -> Graph.Edge (List Label) -> String -> String
edgesToGP2 graph { from, to, label } acc =
    acc
        ++ (Maybe.map2 (\f t -> List.foldl (edgeToGP2 f t) "" label) (Graph.get from graph) (Graph.get to graph)
                |> Maybe.withDefault ""
           )


nodeToDot : Graph.Node ( Label, Ellipse ) -> Stmt
nodeToDot node =
    let
        center =
            (Tuple.second node.label).center
    in
    NodeStmt
        (DotLang.NodeId (ID (Tuple.first node.label).id) Nothing)
        [ Attr (ID "pos") (ID (String.fromFloat center.x ++ "," ++ String.fromFloat center.y ++ "!"))
        , Attr (ID "label") (ID (Tuple.first node.label).label)
        , Attr (ID "fillcolor") (ID (markToColour (Tuple.first node.label).mark |> Tuple.second))
        , Attr (ID "style") (ID ("filled"++(if (Tuple.first node.label).flag then ",bold" else "")))
        ]


edgeToDot : VisualGraph -> Graph.Edge (List Label) -> Maybe (List Stmt)
edgeToDot graph edge =
    let
        from =
            Graph.get edge.from graph
                |> Maybe.map (.node >> .label >> Tuple.first >> .id)

        to =
            Graph.get edge.to graph
                |> Maybe.map (.node >> .label >> Tuple.first >> .id)
    in
    Maybe.map2
        ( \f t ->
            List.map
                ( \l ->
                    EdgeStmtNode
                        (DotLang.NodeId (ID f) Nothing)
                        (EdgeNode (DotLang.NodeId (ID t) Nothing))
                        []
                        [ Attr (ID "label") (ID l.label)
                        , Attr (ID "dir") (ID (if l.flag then "both" else "forward"))
                        , Attr (ID "color") (ID (markToColour l.mark |> Tuple.first))
                        , Attr (ID "style") (ID (if l.mark == Dashed then "dashed" else "solid"))
                        , Attr (ID "id") (ID l.id)
                        ]
                )
                edge.label
        )
        from
        to


toGP2 : VisualGraph -> String
toGP2 graph =
    "[\n"
        ++ List.foldl nodesToGP2 "" (Graph.nodes graph)
        ++ "\t|\n"
        ++ List.foldl (edgesToGP2 graph) "" (Graph.edges graph)
        ++ "]"


toDot : VisualGraph -> String
toDot graph =
    Dot
        DotLang.Digraph
        Nothing
        (List.map nodeToDot (Graph.nodes graph) ++ (List.filterMap (edgeToDot graph) (Graph.edges graph) |> List.concat))
        |> DotLang.toString


getCorrespondingNode : Node ( Label, Ellipse ) -> VisualGraph -> Maybe (Node ( Label, Ellipse ))
getCorrespondingNode node graph =
    Graph.nodes graph
        |> List.filter (.label >> Tuple.first >> .id >> (==) (Tuple.first node.label).id)
        |> List.head


getCorrespondingEdge : Label -> VisualGraph -> Maybe Label
getCorrespondingEdge label graph =
    Graph.edges graph
        |> List.map (.label >> List.filter (.id >> (==) label.id))
        |> List.concat
        |> List.head


getAttr : String -> List Attr -> Maybe String
getAttr name attrs =
    List.filterMap
        (\a ->
            case a of
                Attr (ID n) id ->
                    if n == name then getDotId id else Nothing

                _ ->
                    Nothing
        )
        attrs
        |> List.head


getDotLabel : List Attr -> String
getDotLabel attrs =
    getAttr "label" attrs
        |> Maybe.withDefault ""


getDotNodeMark : List Attr -> Mark
getDotNodeMark attrs =
    case getAttr "fillcolor" attrs |> Maybe.withDefault "" of
        "pink" ->
            Any

        "grey" ->
            Grey

        "red" ->
            Red

        "green" ->
            Green

        "blue" ->
            Blue

        _ ->
            None


getDotEdgeMark : List Attr -> Mark
getDotEdgeMark attrs =
    if String.contains "dashed" (getAttr "style" attrs |> Maybe.withDefault "") then
        Dashed

    else
        case getAttr "color" attrs |> Maybe.withDefault "" of
            "pink" ->
                Any

            "red" ->
                Red

            "green" ->
                Green

            "blue" ->
                Blue

            _ ->
                None


getDotRoot : List Attr -> Bool
getDotRoot attrs =
    String.contains "bold" (getAttr "style" attrs |> Maybe.withDefault "")


getDotBidir : List Attr -> Bool
getDotBidir attrs =
    Maybe.map ((==) "both") (getAttr "dir" attrs)
        |> Maybe.withDefault False


parseCenter : String -> Maybe Vec2
parseCenter pos =
    let
        coords =
            String.split "," pos
    in
    Maybe.map2
        (\x y -> { x = x, y = y })
        (List.head coords |> Maybe.map String.trim |> Maybe.andThen String.toFloat)
        (List.tail coords |> Maybe.andThen List.head |> Maybe.map String.trim |> Maybe.map (String.replace "!" "") |> Maybe.andThen String.toFloat)


getDotPos : List Attr -> Maybe Ellipse
getDotPos attrs =
    Maybe.map
        (\center -> { major = 25, minor = 25, center = center})
        (getAttr "pos" attrs |> Maybe.andThen parseCenter)


getDotId : ID -> Maybe String
getDotId id =
    case id of
        ID s ->
            Just s

        NumeralID f ->
            Just (round f |> String.fromInt)

        _ ->
            Nothing


nodeFromDot : Stmt -> VisualGraph -> VisualGraph
nodeFromDot stmt graph =
    case stmt of
        NodeStmt (DotLang.NodeId id _) attrs ->
            Maybe.map2
                (\pos i -> createNode ({ id = i, label = getDotLabel attrs, mark = getDotNodeMark attrs, flag = getDotRoot attrs }, pos) graph)
                (getDotPos attrs)
                (getDotId id)
                |> Maybe.withDefault graph

        _ ->
            graph


edgeFromDot : Stmt -> VisualGraph -> VisualGraph
edgeFromDot stmt graph =
    case stmt of
        EdgeStmtNode (DotLang.NodeId from _) (EdgeNode (DotLang.NodeId to _)) _  attrs ->
            Maybe.map3
                (\i f t -> createEdge { id = i, label = getDotLabel attrs, mark = getDotEdgeMark attrs, flag = getDotBidir attrs } f t graph)
                (getAttr "id" attrs)
                (getDotId from |> Maybe.andThen (\f -> internalId f graph))
                (getDotId to |> Maybe.andThen (\t -> internalId t graph))
                |> Maybe.withDefault graph

        _ ->
            graph


fromDot : Dot -> VisualGraph
fromDot (Dot _ _ stmts) =
    List.foldl nodeFromDot Graph.empty stmts
        |> (\graph -> List.foldl edgeFromDot graph stmts)


internalId : String -> VisualGraph -> Maybe NodeId
internalId id graph =
    Graph.nodes graph
        |> List.filter (.label >> Tuple.first >> .id >> (==) id)
        |> List.head
        |> Maybe.map .id


setLabel : String -> Label -> Label
setLabel label meta =
    { meta | label = label }


setId : String -> Label -> Label
setId id meta =
    { meta | id = id }


setMark : Mark -> Label -> Label
setMark mark meta =
    { meta | mark = mark }


setFlag : Bool -> Label -> Label
setFlag flag meta =
    { meta | flag = flag }


isValid : VisualGraph -> Bool
isValid graph =
    True


nextId : Graph n e -> NodeId
nextId graph =
    Graph.nodeIdRange graph
        |> Maybe.map (Tuple.second >> (+) 1)
        |> Maybe.withDefault 0


tryId : Int -> String -> List String -> String
tryId n prefix ids =
    let
        attempt =
            prefix ++ String.fromInt n
    in
    if List.member attempt ids then
        tryId (n + 1) prefix ids

    else
        attempt


createNode : n -> MultiGraph n e -> MultiGraph n e
createNode label graph =
    Graph.insert
        { node = Node (nextId graph) label
        , incoming = IntDict.empty
        , outgoing = IntDict.empty
        }
        graph


createEdge : e -> NodeId -> NodeId -> MultiGraph n e -> MultiGraph n e
createEdge label from to graph =
    Graph.update
        from
        ((::) label |> updateOutgoing to |> Maybe.map)
        graph


deleteEdge : NodeId -> NodeId -> Int -> MultiGraph n e -> MultiGraph n e
deleteEdge from to i graph =
    Graph.update
        from
        (List.Extra.removeAt i |> updateOutgoing to |> Maybe.map)
        graph


updateNode : (n -> n) -> MultiContext n e -> MultiContext n e
updateNode f ({ node } as context) =
    { context | node = { node | label = f node.label } }


updateOutgoing : NodeId -> (List e -> List e) -> MultiContext n e -> MultiContext n e
updateOutgoing to f context =
    { context | outgoing = IntDict.update to (updateMultiEdge f) context.outgoing }


updateMultiEdge : (List e -> List e) -> Maybe (List e) -> Maybe (List e)
updateMultiEdge f edges =
    let
        newEdges =
            Maybe.withDefault [] edges |> f
    in
    if newEdges == [] then
        Nothing

    else
        Just newEdges


nodePosition : NodeId -> VisualGraph -> Maybe Vec2
nodePosition id graph =
    Graph.get id graph |> Maybe.map (.node >> .label >> Tuple.second >> .center)


setNodePosition : NodeId -> Vec2 -> VisualGraph -> VisualGraph
setNodePosition id position graph =
    Graph.update
        id
        (Ellipse.setCenter position |> Tuple.mapSecond |> updateNode |> Maybe.map)
        graph


setNodeMajor : NodeId -> Float -> VisualGraph -> VisualGraph
setNodeMajor id major graph =
    Graph.update
        id
        (Ellipse.setMajor major |> Tuple.mapSecond |> updateNode |> Maybe.map)
        graph


setNodeMinor : NodeId -> Float -> VisualGraph -> VisualGraph
setNodeMinor id minor graph =
    Graph.update
        id
        (Ellipse.setMinor minor |> Tuple.mapSecond |> updateNode |> Maybe.map)
        graph


updateNodeLabel : NodeId -> String -> VisualGraph -> VisualGraph
updateNodeLabel id label graph =
    Graph.update
        id
        (Tuple.mapFirst (setLabel label) |> updateNode |> Maybe.map)
        graph


updateEdgeLabel : NodeId -> NodeId -> Int -> String -> VisualGraph -> VisualGraph
updateEdgeLabel from to id label graph =
    Graph.update
        from
        (List.Extra.updateAt id (setLabel label) |> updateOutgoing to |> Maybe.map)
        graph


updateNodeId : NodeId -> String -> VisualGraph -> VisualGraph
updateNodeId node id graph =
    Graph.update
        node
        (Tuple.mapFirst (setId id) |> updateNode |> Maybe.map)
        graph


updateEdgeId : NodeId -> NodeId -> Int -> String -> VisualGraph -> VisualGraph
updateEdgeId from to edge id graph =
    Graph.update
        from
        (List.Extra.updateAt edge (setId id) |> updateOutgoing to |> Maybe.map)
        graph


updateNodeMark : NodeId -> Mark -> VisualGraph -> VisualGraph
updateNodeMark id mark graph =
    Graph.update
        id
        (Tuple.mapFirst (setMark mark) |> updateNode |> Maybe.map)
        graph


updateEdgeMark : NodeId -> NodeId -> Int -> Mark -> VisualGraph -> VisualGraph
updateEdgeMark from to id mark graph =
    Graph.update
        from
        (List.Extra.updateAt id (setMark mark) |> updateOutgoing to |> Maybe.map)
        graph


updateNodeFlag : NodeId -> Bool -> VisualGraph -> VisualGraph
updateNodeFlag id flag graph =
    Graph.update
        id
        (Tuple.mapFirst (setFlag flag) |> updateNode |> Maybe.map)
        graph


updateEdgeFlag : NodeId -> NodeId -> Int -> Bool -> VisualGraph -> VisualGraph
updateEdgeFlag from to id flag graph =
    Graph.update
        from
        (List.Extra.updateAt id (setFlag flag) |> updateOutgoing to |> Maybe.map)
        graph


getNodeData : NodeId -> VisualGraph -> Maybe Label
getNodeData id graph =
    Graph.get id graph
        |> Maybe.map (.node >> .label >> Tuple.first)


getEdgeData : NodeId -> NodeId -> Int -> VisualGraph -> Maybe Label
getEdgeData from to id graph =
    Graph.get from graph
        |> Maybe.map .outgoing
        |> Maybe.andThen (IntDict.get to)
        |> Maybe.andThen (List.Extra.getAt id)
