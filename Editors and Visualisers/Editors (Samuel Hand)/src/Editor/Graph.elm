module Editor.Graph exposing (Model, Msg(..), Selection(..), Action(..), init, update, view, subscriptions, setGraph, setGraphP)


import AssocList as Dict exposing (Dict)
import Browser.Dom as Dom
import DotLang
import GP2Graph.Dot as Dot
import GP2Graph.GP2Graph as GP2Graph exposing (VisualGraph)
import GP2Graph.GP2Parser as GP2Parser
import DotLang exposing (Dot)
import Geometry.Arc as Arc exposing (Arc)
import Geometry.Ellipse as Ellipse exposing (Ellipse)
import Geometry.LineSegment as LineSegment exposing (LineSegment)
import Geometry.Vec2 as Vec2 exposing (Vec2)
import Graph exposing (Edge, Graph, Node, NodeId)
import Graphics.Colour as Colour exposing (Colour)
import Html
import Html.Attributes
import Html.Events
import IntDict exposing (IntDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra
import Ports.Editor
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import File.Download as Download
import File exposing (File)
import File.Select as Select
import Task


type Selection
    = NodeSelection NodeId
    | EdgeSelection NodeId NodeId Int
    | NullSelection


type Action
    = MoveNode NodeId Vec2 Bool
    | DrawEdge NodeId Vec2
    | Pan Vec2
    | NullAction


type Msg
    = Create Vec2
    | CreateEdge NodeId
    | Move Vec2
    | Action Action
    | Select Selection
    | Delete Selection
    | Fit NodeId Float
    | ActualFit NodeId Float
    | UpdateId Selection String
    | UpdateLabel Selection String
    | UpdateMark Selection GP2Graph.Mark
    | UpdateFlag Selection Bool
    | Zoom Float
    | NullMsg


type alias Model =
    { graph : VisualGraph
    , action : Action
    , selection : Selection
    , id : String
    , host : Bool
    , error : Bool
    , center : Vec2
    , scale : Float
    , width : Float
    , height : Float
    }


type Style
    = StrokeDefault
    | StrokeSelected
    | StrokeFade
    | FillDefault
    | MarkAny
    | MarkGrey
    | MarkRed
    | MarkGreen
    | MarkBlue


arrowSide : Float
arrowSide =
    10


arrowAltitude : Float
arrowAltitude =
    sqrt 3 / 2 * arrowSide


radius : Float
radius =
    25


init : VisualGraph -> String -> Bool -> ( Model, Cmd Msg )
init graph id host =
    let
        model =
            { graph = graph
            , action = NullAction
            , selection = NullSelection
            , id = id
            , host = host
            , error = False
            , center = { x = 375, y = 375 }
            , scale = 1
            , width = 750
            , height = 750
            }

        commands =
            Ports.Editor.editorInit (Encode.string id)
                :: List.map
                    (.id >> fitLabel id)
                    (Graph.nodes graph)
    in
    ( model, Cmd.batch commands )


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.Editor.fitDone
        (Decode.decodeValue ((Decode.field "svg" Decode.string)
            |> Decode.andThen (\s -> if s /= model.id then Decode.fail "" else (Decode.map2 ActualFit (Decode.field "id" Decode.int) (Decode.field "width" Decode.float)))) >> Result.withDefault NullMsg)


update : String -> String -> Msg -> Model -> ( Model, Cmd Msg )
update nextNodeId nextEdgeId msg model =
    case msg of
        Create coords ->
            ( { model | graph = createNode nextNodeId coords model.graph }, Cmd.none )

        CreateEdge to ->
            ( shouldCreateEdge nextEdgeId to model, Cmd.none )

        Move coords ->
            ( mouseMoved coords model, Cmd.none )

        Action action ->
            ( { model
                | action = action
                , selection = shouldSelect action model.selection
              }
            , Cmd.none
            )

        Select selection ->
            ( { model | selection = selection }, Cmd.none )

        Delete selection ->
            ( { model
                | graph = delete selection model.graph
                , selection = shouldDeselect selection model.selection
              }
            , Cmd.none
            )

        Fit id width ->
            ( model, Ports.Editor.findFit (Encode.object [  ("svg", Encode.string model.id), ("id", Encode.int id), ("width", Encode.float width) ]))

        ActualFit id width ->
            ( { model
                | graph = GP2Graph.setNodeMajor id (Basics.max radius ((width+15) / 2)) model.graph
                    |> GP2Graph.setNodeMinor id radius
              }
            , Cmd.none
            )

        UpdateId selection id ->
            ( { model
                | graph = setId selection id model.graph
              }
            , Cmd.none
            )

        UpdateLabel selection label ->
            ( { model
                | graph = setLabel selection label model.graph
              }
            , shouldFit selection model.id
            )

        UpdateMark selection mark ->
            ( { model
                | graph = setMark selection mark model.graph
              }
            , Cmd.none
            )

        UpdateFlag selection flag ->
            ( { model
                | graph = setFlag selection flag model.graph
              }
            , Cmd.none
            )

        Zoom amount ->
            ( { model | scale = model.scale * amount }, Cmd.none )

        NullMsg ->
            ( model, Cmd.none )


stretch : VisualGraph -> VisualGraph
stretch graph =
    Graph.mapNodes (Tuple.mapSecond (\ellipse -> { ellipse | center = Vec2.mul 2 ellipse.center })) graph


setGraph : Bool -> Dot -> Model -> (Model, Cmd Msg)
setGraph shouldStretch dotGraph model =
    setGraphP shouldStretch (GP2Graph.fromDot dotGraph) model


setGraphP : Bool -> VisualGraph -> Model -> (Model, Cmd Msg)
setGraphP shouldStretch parsed model =
    let
        graph =
            if shouldStretch then
                stretch parsed

            else
                parsed

        dim =
            getDim model.host graph
        in
        ({ model | graph = graph, selection = NullSelection, action = NullAction, width = dim.width, height = dim.height, center = dim.center, scale = 1 }, Cmd.batch (List.map (.id >> fitLabel model.id) (Graph.nodes graph)))


getDim : Bool -> VisualGraph -> { width: Float, height : Float, center : Vec2 }
getDim host graph =
    let
        maxX =
            List.map (.label >> Tuple.second >> .center >> .x) (Graph.nodes graph) |> List.maximum |> Maybe.withDefault 650

        maxY =
            List.map (.label >> Tuple.second >> .center >> .y) (Graph.nodes graph) |> List.maximum |> Maybe.withDefault 650

        minX =
            List.map (.label >> Tuple.second >> .center >> .x) (Graph.nodes graph) |> List.minimum |> Maybe.withDefault -100

        minY =
            List.map (.label >> Tuple.second >> .center >> .y) (Graph.nodes graph) |> List.minimum |> Maybe.withDefault -100

        idealWidth =
            Basics.max (maxX - minX) 550

        idealHeight =
            Basics.max (maxY - minY) 550
    in
    { width = idealWidth+200, height = idealHeight+200, center = { x = (minX+maxX)/2, y = (minY+maxY)/2 } }


fitLabel : String -> NodeId -> Cmd Msg
fitLabel id nodeid =
    String.fromInt nodeid
        |> (++) id
        |> Dom.getElement
        |> Task.attempt
            (Result.map (.element >> .width >> Fit nodeid)
                >> Result.withDefault NullMsg
            )


shouldFit : Selection -> String -> Cmd Msg
shouldFit selection id =
    case selection of
        NodeSelection nodeid ->
            fitLabel id nodeid

        _ ->
            Cmd.none


setLabel : Selection -> String -> VisualGraph -> VisualGraph
setLabel selection label graph =
    case selection of
        NodeSelection id ->
            GP2Graph.updateNodeLabel id label graph

        EdgeSelection from to id ->
            GP2Graph.updateEdgeLabel from to id label graph

        NullSelection ->
            graph


setId : Selection -> String -> VisualGraph -> VisualGraph
setId selection id graph =
    case selection of
        NodeSelection node ->
            GP2Graph.updateNodeId node id graph

        EdgeSelection from to edge ->
            GP2Graph.updateEdgeId from to edge id graph

        NullSelection ->
            graph


setMark : Selection -> GP2Graph.Mark -> VisualGraph -> VisualGraph
setMark selection mark graph =
    case selection of
        NodeSelection id ->
            GP2Graph.updateNodeMark id mark graph

        EdgeSelection from to id ->
            GP2Graph.updateEdgeMark from to id mark graph

        NullSelection ->
            graph


setFlag : Selection -> Bool -> VisualGraph -> VisualGraph
setFlag selection flag graph =
    case selection of
        NodeSelection id ->
            GP2Graph.updateNodeFlag id flag graph

        EdgeSelection from to id ->
            GP2Graph.updateEdgeFlag from to id flag graph

        NullSelection ->
            graph


mouseMoved : Vec2 -> Model -> Model
mouseMoved coords model =
    case model.action of
        MoveNode id offset _ ->
            { model | graph = GP2Graph.setNodePosition id (Vec2.add coords offset) model.graph }

        DrawEdge id _ ->
            { model | action = DrawEdge id coords }

        Pan start ->
            { model | center = (Vec2.sub start coords) |> Vec2.add model.center }

        NullAction ->
            model


shouldCreateEdge : String -> NodeId -> Model -> Model
shouldCreateEdge id to model =
    case model.action of
        DrawEdge from _ ->
            { model | graph = createEdge id from to model.graph }

        _ ->
            model


shouldDeselect : Selection -> Selection -> Selection
shouldDeselect deleted current =
    if deleted == current then
        NullSelection

    else
        current


shouldSelect : Action -> Selection -> Selection
shouldSelect action selection =
    case action of
        MoveNode id _ True ->
            NodeSelection id

        Pan _ ->
            NullSelection

        _ ->
            selection


newLabel : String -> GP2Graph.Label
newLabel id =
    { label = ""
    , id = id
    , mark = GP2Graph.None
    , flag = False
    }


createNode : String -> Vec2 -> VisualGraph -> VisualGraph
createNode id coords graph =
    GP2Graph.createNode
        ( newLabel id, Ellipse.fromCircle radius coords )
        graph


createEdge : String -> NodeId -> NodeId -> VisualGraph -> VisualGraph
createEdge id from to graph =
    GP2Graph.createEdge
        (newLabel id)
        from
        to
        graph


delete : Selection -> VisualGraph -> VisualGraph
delete selection graph =
    case selection of
        NodeSelection id ->
            Graph.remove id graph

        EdgeSelection from to id ->
            GP2Graph.deleteEdge from to id graph

        NullSelection ->
            graph


view : List String -> List String -> Model -> Html.Html Msg
view interface interfaceEdges model =
    (Dict.map (\k v -> arrow True k (v ++ "-start")) markerIds |> Dict.values)
        ++ (Dict.map (\k v -> arrow False k (v ++ "-end")) markerIds |> Dict.values)
        ++ List.concatMap (makeEdges interfaceEdges model) (Graph.edges model.graph)
        ++ List.map (makeNode interface model) (Graph.nodes model.graph)
        ++ drawingEdge model
        |> svgContainer model


coordsDecoder : Decoder Vec2
coordsDecoder =
    Decode.map2 Vec2
        (Decode.field "detail" (Decode.field "x" Decode.float))
        (Decode.field "detail" (Decode.field "y" Decode.float))


yScrollDecoder : Decoder Float
yScrollDecoder =
    Decode.field "deltaY" Decode.float
        |> Decode.map (\a -> (if a == 0 then 1 else (if a > 0 then 1/(0.75) else 0.75)))


createDecoder : Decoder Msg
createDecoder =
    Decode.map Create coordsDecoder


deleteDecoder : Msg -> Decoder Msg
deleteDecoder msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Delete" then
                    Decode.succeed msg

                else
                    Decode.fail "Non-delete key pressed - doing nothing"
            )


startMoveDecoder : Node ( GP2Graph.Label, Ellipse ) -> Decoder Msg
startMoveDecoder node =
    coordsDecoder
        |> Decode.map
            (Vec2.sub (Tuple.second node.label).center
                >> (\offset -> MoveNode node.id offset True)
                >> Action
            )


startEdgeDecoder : NodeId -> Decoder Msg
startEdgeDecoder nodeId =
    coordsDecoder
        |> Decode.map (DrawEdge nodeId >> Action)


moveDecoder : Decoder Msg
moveDecoder =
    Decode.map Move coordsDecoder


svgContainer : Model -> List (Svg Msg) -> Html.Html Msg
svgContainer model contents =
    let
        width =
            model.width * model.scale

        height =
            model.height * model.scale

        x =
            model.center.x - (width/2)

        y =
            model.center.y - (height/2)
    in
    svg
        [ class ("graph-editor"++(if model.id == "left-rule-editor" then " border-right" else ""))
        , on "svgrightup" (Decode.succeed (Action NullAction))
        , on "svgleftup" (Decode.succeed (Action NullAction))
        , on "svgmousemove" moveDecoder
        , on "mouseleave" (Decode.succeed (Action NullAction))
        , on "svgleftdown" (coordsDecoder |> Decode.map (Pan >> Action))
        , on "svgdblclick" createDecoder
        , on "wheel" (yScrollDecoder |> Decode.map Zoom)
        , id model.id
        , Html.Attributes.attribute "tabindex" "0"
        , on "keydown" (deleteDecoder (Delete model.selection))
        , viewBox ((String.fromFloat x)++" "++(String.fromFloat y)++" "++(String.fromFloat width)++" "++(String.fromFloat height))
        , preserveAspectRatio "xMidYMid meet"
        , Svg.Attributes.cursor
            (case model.action of
                Pan _ ->
                    "move"
                _ ->
                    "auto"
            )
        ]
        contents


arrow : Bool -> Style -> String -> Svg Msg
arrow start style name =
    marker
        [ id <| "arrow-" ++ name
        , refX <| String.fromFloat (if start then 0 else arrowAltitude)
        , refY <| String.fromFloat <| 0.5 * arrowSide
        , markerWidth <| String.fromFloat <| 2 * arrowSide
        , markerHeight <| String.fromFloat <| 2 * arrowSide
        , orient "auto"
        ]
        [ polygon
            [ points <|
                String.fromFloat (if start then arrowAltitude else 0)
                    ++ ","
                    ++ String.fromFloat arrowSide
                    ++ " "
                    ++ String.fromFloat (if start then arrowAltitude else 0)
                    ++ ",0 "
                    ++ String.fromFloat (if start then 0 else arrowAltitude)
                    ++ ","
                    ++ (String.fromFloat <| 0.5 * arrowSide)
            , fill (colour style |> Colour.toCss)
            ]
            []
        ]


makeNode : List String -> Model -> Node ( GP2Graph.Label, Ellipse ) -> Svg Msg
makeNode interfaceNodes model node =
    let
        shape =
            Tuple.second node.label

        interface =
            List.member (Tuple.first node.label).id interfaceNodes

        style =
            nodeStyle interface model node

        events =
            [ stopPropagationOn "svgleftdown" (startMoveDecoder node |> Decode.map (\a -> (a, True)))
            , on "svgrightdown" (startEdgeDecoder node.id)
            , stopPropagationOn "click" (Decode.succeed (NullMsg, True))
            , stopPropagationOn "svgdblclick" (Decode.succeed (NullMsg, True))
            , on "svgrightup" (Decode.succeed (CreateEdge node.id))
            ]
    in
    g
        []
        [ ellipse (Ellipse.toSvg shape ++ style ++ events) []
        , text_
            [ x (String.fromFloat shape.center.x)
            , y (String.fromFloat (shape.center.y + shape.minor + 15))
            , fontSize "15"
            , textAnchor "middle"
            , pointerEvents "none"
            , fontWeight (if interface  && (not model.host) then "bold" else "normal")
            ]
            [ text (if model.host then "" else (Tuple.first node.label).id) ]
        , text_
            [ x (String.fromFloat shape.center.x)
            , y (String.fromFloat shape.center.y)
            , fontSize "15"
            , textAnchor "middle"
            , pointerEvents "none"
            , dominantBaseline "central"
            , id (model.id ++ String.fromInt node.id)
            ]
            [ text (if (String.trim (Tuple.first node.label).label) == "empty" then "" else (Tuple.first node.label).label) ]
        ]


makeEdges : List String -> Model -> Edge (List GP2Graph.Label) -> List (Svg Msg)
makeEdges interface model edges =
    List.indexedMap
        (makeEdge interface model edges.from edges.to)
        edges.label


makeEdge : List String -> Model -> NodeId -> NodeId -> Int -> GP2Graph.Label -> Svg Msg
makeEdge interfaceEdges model from to id edge =
    let
        interface =
            List.member edge.id interfaceEdges

        maybeShape =
            if from == to then
                Maybe.map
                    (reflexivePath model id)
                    (Graph.get from model.graph)

            else
                Maybe.map2
                    (edgePath model id)
                    (Graph.get from model.graph)
                    (Graph.get to model.graph)

        visibleStyle =
            edgeStyle interface model from to id edge

        hiddenStyle =
            [ strokeWidth "15"
            , visibility "hidden"
            ]

        visibleEvents =
            [ pointerEvents "none"
            ]

        hiddenEvents =
            [ stopPropagationOn "svgleftdown" (Decode.succeed ((Select (EdgeSelection from to id)), True))
            , stopPropagationOn "svgdblclick" (Decode.succeed (NullMsg, True))
            , pointerEvents "stroke"
            ]
    in
    case maybeShape of
        Nothing ->
            Svg.text "unknown"

        Just shape ->
            g
                []
                [ Svg.path (Arc.toSvg shape ++ visibleEvents ++ visibleStyle) []
                , Svg.path (Arc.toSvg shape ++ hiddenEvents ++ hiddenStyle) []
                , text_
                    [ fontSize "15"
                    , pointerEvents "none"
                    , x (String.fromFloat (Arc.peak shape).x)
                    , y (String.fromFloat (Arc.peak shape).y)
                    , dy "-10"
                    , textAnchor "middle"
                    , transform
                        ("rotate("
                            ++ String.fromFloat (Vec2.angle shape.start shape.end)
                            ++ ","
                            ++ String.fromFloat (Arc.peak shape).x
                            ++ ","
                            ++ String.fromFloat (Arc.peak shape).y
                            ++ ")"
                        )
                    ]
                    [ text edge.label ]
                ]


drawingEdge : Model -> List (Svg Msg)
drawingEdge { graph, action } =
    case action of
        DrawEdge id coords ->
            Maybe.map
                (edgeToPoint coords)
                (Graph.get id graph)
                |> Maybe.withDefault []

        _ ->
            []


edgeToPoint : Vec2 -> GP2Graph.VisualContext -> List (Svg Msg)
edgeToPoint coords { node } =
    let
        nodeEllipse =
            Tuple.second node.label

        path =
            LineSegment.toSvg
                { start = Ellipse.project coords nodeEllipse
                , end = coords
                }

        lineEnd =
            Vec2.direction nodeEllipse.center coords
                |> Vec2.mul arrowAltitude
                |> Vec2.sub coords

        style =
            [ strokeWidth "1"
            , pointerEvents "none"
            , stroke <| Colour.toCss <| colour StrokeDefault
            , markerEnd
                (Dict.get StrokeDefault markerIds
                    |> Maybe.map (\marker -> "url(#arrow-" ++ marker ++ "-end)")
                    |> Maybe.withDefault "none"
                )
            ]
    in
    if not (Ellipse.contains lineEnd nodeEllipse) then
        [ line (path ++ style) [] ]

    else
        []


edgePath : Model -> Int -> GP2Graph.VisualContext -> GP2Graph.VisualContext -> Arc
edgePath { graph } id from to =
    let
        fromEllipse =
            from.node.label |> Tuple.second

        toEllipse =
            to.node.label |> Tuple.second

        line =
            Ellipse.lineBetween fromEllipse toEllipse

        intersects =
            Graph.nodes graph
                |> List.filter
                    (\node -> node /= from.node && node /= to.node)
                |> List.any
                    (.label >> Tuple.second >> Ellipse.intersects line)

        bidir =
            IntDict.member from.node.id to.outgoing

        bend =
            if intersects || bidir then
                id + 1

            else
                id + 0
    in
    { start = line.start
    , end = line.end
    , major = LineSegment.length line
    , minor = toFloat bend * radius * 10
    , sweep = False
    }


reflexivePath : Model -> Int -> GP2Graph.VisualContext -> Arc
reflexivePath { graph } id { node } =
    let
        start =
            Tuple.second node.label |> Ellipse.upperArc |> Tuple.first

        end =
            Tuple.second node.label |> Ellipse.upperArc |> Tuple.second
    in
    { start = start
    , end = end
    , major = 0.6 * Vec2.distance start end * ((toFloat id * 0.05) + 1)
    , minor = radius * ((toFloat id * 0.6) + 1)
    , sweep = True
    }


colour : Style -> Colour
colour style =
    case style of
        StrokeDefault ->
            { r = 0, g = 0, b = 0, a = 1.0 }

        StrokeFade ->
            { r = 150, g = 150, b = 150, a = 1.0 }

        StrokeSelected ->
            { r = 222, g = 145, b = 22, a = 1.0 }

        FillDefault ->
            { r = 255, g = 255, b = 255, a = 1.0 }

        MarkAny ->
            { r = 235, g = 72, b = 148, a = 1.0 }

        MarkGrey ->
            { r = 184, g = 184, b = 184, a = 1.0 }

        MarkRed ->
            { r = 235, g = 46, b = 66, a = 1.0 }

        MarkGreen ->
            { r = 35, g = 111, b = 98, a = 1.0 }

        MarkBlue ->
            { r = 85, g = 170, b = 235, a = 1.0 }

markerIds : Dict Style String
markerIds =
    Dict.fromList
        [ ( StrokeDefault, "default" )
        , ( StrokeSelected, "selected" )
        , ( StrokeFade, "fade" )
        , ( MarkAny, "any" )
        , ( MarkRed, "red" )
        , ( MarkGreen, "green" )
        , ( MarkBlue, "blue" )
        ]


markToStyle : Style -> GP2Graph.Mark -> Style
markToStyle default mark =
    case mark of
        GP2Graph.Any ->
            MarkAny

        GP2Graph.Grey ->
            MarkGrey

        GP2Graph.Red ->
            MarkRed

        GP2Graph.Green ->
            MarkGreen

        GP2Graph.Blue ->
            MarkBlue

        _ ->
            default


nodeStyle : Bool -> Model -> Node ( GP2Graph.Label, Ellipse ) -> List (Attribute Msg)
nodeStyle interface { selection, host } node =
    let
        fillColour =
            markToStyle FillDefault (Tuple.first node.label).mark |> colour

        strokeColour =
            if selection == NodeSelection node.id then
                colour StrokeSelected

            else
                if not (interface || host) then
                    Colour.brighten 150 (colour StrokeDefault)

                else
                    colour StrokeDefault
    in
    [ strokeWidth (if (Tuple.first node.label).flag then "4" else "2")
    , fill <| Colour.toCss fillColour
    , stroke <| Colour.toCss strokeColour
    ]


edgeStyle : Bool -> Model -> NodeId -> NodeId -> Int -> GP2Graph.Label -> List (Attribute Msg)
edgeStyle interface { selection, host } from to id edge =
    let
        strokeStyle =
            if selection == EdgeSelection from to id then
                StrokeSelected

            else
                markToStyle (if not host && not interface then StrokeFade else StrokeDefault) edge.mark

        markerId suffix =
            Dict.get strokeStyle markerIds
                |> Maybe.map (\style -> "url(#arrow-" ++ style ++ suffix ++ ")")
                |> Maybe.withDefault "none"

        dashed =
            if edge.mark == GP2Graph.Dashed then
                "10 5"

            else
                "0"
    in
    [ strokeWidth (if not host && interface then "2" else "1")
    , stroke (colour strokeStyle |> Colour.toCss)
    , fillOpacity "0"
    , markerEnd (if edge.flag then "none" else markerId "-end")
    , strokeDasharray dashed
    ]
