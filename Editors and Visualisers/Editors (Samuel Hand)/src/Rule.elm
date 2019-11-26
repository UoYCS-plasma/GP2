module Rule exposing (main)

import Browser exposing (Document)
import Editor.Graph as GraphEditor
import Editor.Element as ElementEditor exposing (FileMsg(..))
import File
import GP2Graph.Dot as Dot
import Ports.Editor
import Geometry.Vec2 as Vec2 exposing (Vec2)
import Json.Encode as Encode
import File.Download as Download
import Task
import DotLang exposing (Dot)
import GP2Graph.GP2Parser as GP2Parser
import GP2Graph.RuleParser as RuleParser
import Json.Decode as Decode
import Parser
import File.Select as Select
import List.Extra
import GP2Graph.GP2Graph as GP2Graph exposing (VisualGraph)
import GP2Graph.GP2Rule as GP2Rule
import Graph
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { rightModel : GraphEditor.Model
    , leftModel : GraphEditor.Model
    , vars : String
    , condition : String
    , elementModel : ElementEditor.Model
    , error : Bool
    }


type Msg
    = Left ElementEditor.Msg
    | Right ElementEditor.Msg
    | SetVars String
    | SetCondition String


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( rightModel, rightCmd ) =
            GraphEditor.init Graph.empty "right-rule-editor" False

        ( leftModel, leftCmd ) =
            GraphEditor.init Graph.empty "left-rule-editor" False

    in
    ( { rightModel = rightModel, leftModel = leftModel, elementModel = ElementEditor.init, error = False, vars = "", condition = "" }, Cmd.batch [ Cmd.map (ElementEditor.EditorMsg >> Right) rightCmd, Cmd.map (ElementEditor.EditorMsg >> Left) leftCmd ] )


view : Model -> Document Msg
view model =
    { title = "gp2editor"
    , body =
        [ div
            [ class "host-container" ]
            [ ElementEditor.view (getActiveEditor model) (getInactiveEditor model).graph model.elementModel
                |> Html.map (getActiveMsg model)
            , input [ class "ruledecl border-bottom", type_ "text", placeholder "name ( vars: type, ... )", value model.vars, onInput SetVars ] []
            , div
                [ class "rule-container" ]
                [ GraphEditor.view
                        (getNodeIds model.rightModel.graph)
                        (getEdgeIds model.rightModel.graph)
                        model.leftModel
                        |> Html.map (ElementEditor.EditorMsg >> Left)
                , GraphEditor.view
                    (getNodeIds model.leftModel.graph)
                    (getEdgeIds model.leftModel.graph)
                    model.rightModel
                    |> Html.map (ElementEditor.EditorMsg >> Right)
                ]
            , input [ type_ "text", class "condition border-top", placeholder "Condition", value model.condition, onInput SetCondition ] []
            ]
        ]++(showModal model.error)
    }


getNodeIds : GP2Graph.VisualGraph -> List String
getNodeIds graph =
    Graph.nodes graph
        |> List.map (.label >> Tuple.first >> .id)


getEdgeIds : GP2Graph.VisualGraph -> List String
getEdgeIds graph =
    Graph.edges graph
        |> List.concatMap (.label >> List.map .id)


getActiveEditor : Model -> GraphEditor.Model
getActiveEditor { leftModel, rightModel } =
    if leftModel.selection /= GraphEditor.NullSelection then
        leftModel

    else
        rightModel


getInactiveEditor : Model -> GraphEditor.Model
getInactiveEditor { leftModel, rightModel } =
    if leftModel.selection /= GraphEditor.NullSelection then
        rightModel

    else
        leftModel


getActiveMsg : Model -> (ElementEditor.Msg -> Msg)
getActiveMsg { leftModel } =
    if leftModel.selection /= GraphEditor.NullSelection then
        Left

    else
        Right


showModal : Bool -> List (Html Msg)
showModal show =
    if show then
        [ div
            [ class "modal"
            , tabindex -1
            , attribute "role" "dialog"
            , style "display" "block"
            ]
            [ div
                [ class "modal-dialog", attribute "role" "document" ]
                [ div
                    [ class "modal-content" ]
                    [ div
                        [ class "modal-header" ]
                        [ h5 [ class "modal-title" ] [ text "Error Parsing File" ]
                        , button [ type_ "button", class "close", onClick (Right (ElementEditor.FileMsg DismissError)) ] [ text "×" ]
                        ]
                    , div
                        [ class "modal-body" ]
                        [ text "File invalid, please try a different file" ]
                    , div
                        [ class "modal-footer" ]
                        [ button [ type_ "button", class "btn btn-primary", onClick (Left (ElementEditor.FileMsg DismissError)) ] [ text "OK" ]]
                    ]
                ]
            ]
        , div [ class "modal-backdrop show", id "error-modal-back" ] []
        ]

    else
        []


isInterface : Graph.NodeId -> GP2Graph.VisualGraph -> GP2Graph.VisualGraph -> Bool
isInterface id graph otherGraph =
    case Graph.get id graph of
        Just { node } ->
            List.member (Tuple.first node.label).id (getNodeIds otherGraph)

        _ ->
            False


correspondingMove : Graph.NodeId -> Vec2 -> VisualGraph -> VisualGraph -> Maybe GraphEditor.Msg
correspondingMove id offset graph otherGraph =
    case Graph.get id graph of
        Just { node } ->
            Maybe.map
                (\mId -> GraphEditor.Action (GraphEditor.MoveNode mId offset False))
                (GP2Graph.internalId (Tuple.first node.label).id otherGraph)

        _ ->
            Nothing


correspondingDraw : Graph.NodeId -> Vec2 -> VisualGraph -> VisualGraph -> Maybe GraphEditor.Msg
correspondingDraw id start graph otherGraph =
    case Graph.get id graph of
        Just { node } ->
            Maybe.map
                (\mId -> GraphEditor.Action (GraphEditor.DrawEdge mId start))
                (GP2Graph.internalId (Tuple.first node.label).id otherGraph)

        _ ->
            Nothing


correspondingEdge : Graph.NodeId -> VisualGraph -> VisualGraph -> Maybe GraphEditor.Msg
correspondingEdge id graph otherGraph =
    case Graph.get id graph of
        Just { node } ->
            Maybe.map
                GraphEditor.CreateEdge
                (GP2Graph.internalId (Tuple.first node.label).id otherGraph)

        _ ->
            Nothing


correspondingId : Graph.NodeId -> String -> VisualGraph -> VisualGraph -> Maybe GraphEditor.Msg
correspondingId nodeId id graph otherGraph =
    case Graph.get nodeId graph of
        Just { node } ->
            Maybe.map
                (\mId -> GraphEditor.UpdateId (GraphEditor.NodeSelection mId) id)
                (GP2Graph.internalId (Tuple.first node.label).id otherGraph)

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nextNodeId =
            GP2Graph.tryId 0 "n" ((Graph.nodes model.rightModel.graph)++(Graph.nodes model.leftModel.graph) |> List.map (.label >> Tuple.first >> .id))

        nextEdgeId =
            GP2Graph.tryId 0 "e" ((Graph.edges model.rightModel.graph)++(Graph.edges model.leftModel.graph) |> List.concatMap (.label >> List.map .id))
    in
    case msg of
        Right (ElementEditor.EditorMsg editorMsg) ->
            let
                (newModel, cmd) =
                    updateRight nextNodeId nextEdgeId editorMsg model
                        |> Tuple.mapFirst (updateLeft nextNodeId nextEdgeId otherMsg >> Tuple.first)

                otherMsg =
                    case editorMsg of
                        GraphEditor.Action (GraphEditor.MoveNode id offset _) ->
                            correspondingMove id offset model.rightModel.graph model.leftModel.graph
                                |> Maybe.withDefault GraphEditor.NullMsg

                        GraphEditor.Action GraphEditor.NullAction ->
                            editorMsg

                        GraphEditor.Move _ ->
                            editorMsg

                        GraphEditor.UpdateId (GraphEditor.NodeSelection node) id ->
                            correspondingId node id model.rightModel.graph model.leftModel.graph
                                |> Maybe.withDefault GraphEditor.NullMsg

                        _ ->
                            GraphEditor.NullMsg
            in
            if newModel.rightModel.selection /= model.rightModel.selection then
                (updateElement newModel.rightModel (ElementEditor.Select newModel.rightModel.selection) (Tuple.first (updateLeft nextNodeId nextEdgeId (GraphEditor.Select GraphEditor.NullSelection) newModel)), cmd)

            else
                (newModel, cmd)

        Left (ElementEditor.EditorMsg editorMsg) ->
            let
                otherMsg =
                    case editorMsg of
                        GraphEditor.Create _ ->
                            editorMsg

                        GraphEditor.Action (GraphEditor.MoveNode id offset _) ->
                            correspondingMove id offset model.leftModel.graph model.rightModel.graph
                                |> Maybe.withDefault GraphEditor.NullMsg


                        GraphEditor.Action (GraphEditor.DrawEdge id start) ->
                            correspondingDraw id start model.leftModel.graph model.rightModel.graph
                                |> Maybe.withDefault GraphEditor.NullMsg

                        GraphEditor.CreateEdge id ->
                            correspondingEdge id model.leftModel.graph model.rightModel.graph
                                |> Maybe.withDefault GraphEditor.NullMsg


                        GraphEditor.Action GraphEditor.NullAction ->
                            editorMsg

                        GraphEditor.Move _ ->
                            editorMsg

                        GraphEditor.UpdateId (GraphEditor.NodeSelection node) id ->
                            correspondingId node id model.leftModel.graph model.rightModel.graph
                                |> Maybe.withDefault GraphEditor.NullMsg

                        _ ->
                            GraphEditor.NullMsg

                (newModel, cmd) =
                    updateLeft nextNodeId nextEdgeId editorMsg model
                        |> Tuple.mapFirst (updateRight nextNodeId nextEdgeId otherMsg >> Tuple.first)

            in
            if newModel.leftModel.selection /= model.leftModel.selection then
                (updateElement newModel.leftModel (ElementEditor.Select newModel.leftModel.selection) (Tuple.first (updateRight nextNodeId nextEdgeId (GraphEditor.Select GraphEditor.NullSelection) newModel)), cmd)

            else
                (newModel, cmd)

        Right (ElementEditor.ElementMsg elementMsg) ->
            (updateElement model.rightModel elementMsg model, Cmd.none)

        Left (ElementEditor.ElementMsg elementMsg) ->
            (updateElement model.leftModel elementMsg model, Cmd.none)

        Right (ElementEditor.FileMsg fileMsg) ->
            updateFile fileMsg model

        Left (ElementEditor.FileMsg fileMsg) ->
            updateFile fileMsg model

        SetVars vars ->
            ( { model | vars = vars }, Cmd.none )

        SetCondition condition ->
            ( { model | condition = condition }, Cmd.none )



updateFile : FileMsg -> Model -> ( Model, Cmd Msg )
updateFile msg model =
    case msg of
        OpenGP2 ->
            ( model, Select.file [ ".rule" ] (SelectGP2 >> ElementEditor.FileMsg >> Right) )

        OpenDot ->
            ( model, Select.file [ ".dot" ] (SelectDot >> ElementEditor.FileMsg >> Right) )

        SelectGP2 file ->
            ( model
            , Task.perform
                (LoadGP2 >> ElementEditor.FileMsg >> Right)
                (File.toString file)
            )

        SelectDot file ->
            ( model, Task.perform (LoadDot False >> ElementEditor.FileMsg >> Right) (File.toString file))

        LoadGP2 graph ->
            shouldLayout False (RuleParser.parse graph) model

        LoadDot layoutDone graph ->
            shouldLayout layoutDone (parseGraph layoutDone graph) model

        SaveGP2 ->
            ( model, Download.string "graph.rule" "text/plain" (GP2Rule.toGP2 (toRule model)))

        SaveDot ->
            ( model, Download.string "graph.dot" "text/vnd.graphviz" (GP2Rule.toDot (toRule model)))

        DismissError ->
            ( { model | error = False }, Cmd.none )


toRule : Model -> GP2Rule.GP2Rule
toRule { vars, leftModel, rightModel, condition } =
    { vars = vars, left = leftModel.graph, right = rightModel.graph, condition = condition }


parseGraph : Bool -> String -> Result (List Parser.DeadEnd) Dot
parseGraph shouldFix graph =
    if shouldFix then
        String.lines graph
            |> List.map String.trim
            |> List.Extra.removeAt 1
            |> List.Extra.removeAt 1
            |> String.join " "
            |> String.words
            |> String.join " "
            |> String.replace "\\ " " "
            |> DotLang.fromString

    else
        DotLang.fromString graph


shouldLayout : Bool -> Result (List Parser.DeadEnd) Dot  -> Model -> (Model, Cmd Msg)
shouldLayout layoutDone graph model =
    case graph of
        Ok dotGraph ->
            if layoutDone || not (Dot.needsLayout dotGraph) then
                let
                    rule =
                        GP2Rule.fromDot dotGraph

                    (newModel, rightCmd) =
                        setRightGraph layoutDone rule.right model

                    (newNewModel, leftCmd) =
                        setLeftGraph layoutDone rule.left newModel
                in
                ( { newNewModel | vars = rule.vars, condition = rule.condition }, Cmd.batch [ rightCmd, leftCmd ] )

            else
                ( model, Ports.Editor.layoutGraph (Encode.string (DotLang.toString dotGraph)))

        Err _ ->
            ( { model | error = True }, Cmd.none )


setRightGraph : Bool -> VisualGraph -> Model -> ( Model, Cmd Msg )
setRightGraph shouldStretch graph model =
    let
        ( editorModel, cmd ) =
            GraphEditor.setGraphP shouldStretch graph model.rightModel
    in
    ({ model | rightModel = editorModel, elementModel = ElementEditor.update editorModel (ElementEditor.Select GraphEditor.NullSelection) model.elementModel }, Cmd.map (ElementEditor.EditorMsg >> Right) cmd)


setLeftGraph : Bool -> VisualGraph -> Model -> ( Model, Cmd Msg )
setLeftGraph shouldStretch graph model =
    let
        ( editorModel, cmd ) =
            GraphEditor.setGraphP shouldStretch graph model.leftModel
    in
    ({ model | leftModel = editorModel, elementModel = ElementEditor.update editorModel (ElementEditor.Select GraphEditor.NullSelection) model.elementModel }, Cmd.map (ElementEditor.EditorMsg >> Left) cmd)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ GraphEditor.subscriptions model.rightModel
            |> Sub.map (ElementEditor.EditorMsg >> Right)
        , GraphEditor.subscriptions model.leftModel
            |> Sub.map (ElementEditor.EditorMsg >> Left)
        , Ports.Editor.layoutDone
            (Decode.decodeValue (Decode.string) >> Result.map (LoadDot True >> ElementEditor.FileMsg >> Right) >> Result.withDefault (Right (ElementEditor.EditorMsg GraphEditor.NullMsg)))
        ]


updateRight : String -> String -> GraphEditor.Msg -> Model -> ( Model, Cmd Msg )
updateRight nextNodeId nextEdgeId msg model =
    let
        ( newEditor, cmd ) =
            GraphEditor.update nextNodeId nextEdgeId msg model.rightModel
    in
    ( { model | rightModel = newEditor }, Cmd.map (ElementEditor.EditorMsg >> Right) cmd )


updateLeft : String -> String -> GraphEditor.Msg -> Model -> ( Model, Cmd Msg )
updateLeft nextNodeId nextEdgeId msg model =
    let
        ( newEditor, cmd ) =
            GraphEditor.update nextNodeId nextEdgeId msg model.leftModel
    in
    ( { model | leftModel = newEditor }, Cmd.map (ElementEditor.EditorMsg >> Left) cmd )


updateElement : GraphEditor.Model -> ElementEditor.ElementMsg -> Model -> Model
updateElement graph msg model =
    { model | elementModel = ElementEditor.update graph msg model.elementModel }
