module Main exposing (main)

import Browser exposing (Document)
import Editor.Graph as GraphEditor
import Editor.Element as ElementEditor exposing (Msg(..), FileMsg(..))
import File
import GP2Graph.Dot as Dot
import Ports.Editor
import Json.Encode as Encode
import File.Download as Download
import Task
import DotLang exposing (Dot)
import GP2Graph.GP2Parser as GP2Parser
import Json.Decode as Decode
import Parser
import File.Select as Select
import List.Extra
import GP2Graph.GP2Graph as GP2Graph exposing (VisualGraph)
import Graph
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { editorModel : GraphEditor.Model
    , elementModel : ElementEditor.Model
    , error : Bool
    }


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
        ( editorModel, editorCmd ) =
            GraphEditor.init Graph.empty "host" True

    in
    ( { editorModel = editorModel, elementModel = ElementEditor.init, error = False }, Cmd.map EditorMsg editorCmd )


view : Model -> Document Msg
view model =
    { title = "gp2editor"
    , body =
        [ div
            [ class "host-container" ]
            [ ElementEditor.view model.editorModel Graph.empty model.elementModel
            , GraphEditor.view [] [] model.editorModel
                |> Html.map EditorMsg
            ]
        ]++(showModal model.error)
    }


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
                        , button [ type_ "button", class "close", onClick (FileMsg DismissError) ] [ text "×" ]
                        ]
                    , div
                        [ class "modal-body" ]
                        [ text "File invalid, please try a different file" ]
                    , div
                        [ class "modal-footer" ]
                        [ button [ type_ "button", class "btn btn-primary", onClick (FileMsg DismissError) ] [ text "OK" ]]
                    ]
                ]
            ]
        , div [ class "modal-backdrop show", id "error-modal-back" ] []
        ]

    else
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nextNodeId =
            GP2Graph.tryId 0 "" (Graph.nodes model.editorModel.graph |> List.map (.label >> Tuple.first >> .id))

        nextEdgeId =
            GP2Graph.tryId 0 "" (Graph.edges model.editorModel.graph |> List.concatMap (.label >> List.map .id))
    in
    case msg of
        EditorMsg editorMsg ->
            let
                (newModel, cmd) =
                    updateEditor nextNodeId nextEdgeId editorMsg model
            in
            if newModel.editorModel.selection /= model.editorModel.selection then
                (updateElement (ElementEditor.Select newModel.editorModel.selection) newModel, cmd)

            else
                (newModel, cmd)

        ElementMsg elementMsg ->
            (updateElement elementMsg model, Cmd.none)

        FileMsg fileMsg ->
            updateFile fileMsg model


updateFile : FileMsg -> Model -> ( Model, Cmd Msg )
updateFile msg model =
    case msg of
        OpenGP2 ->
            ( model, Select.file [ ".host" ] (SelectGP2 >> FileMsg) )

        OpenDot ->
            ( model, Select.file [ ".dot" ] (SelectDot >> FileMsg) )

        SelectGP2 file ->
            ( model
            , Task.perform
                (LoadGP2 >> FileMsg)
                (File.toString file)
            )

        SelectDot file ->
            ( model, Task.perform (LoadDot False >> FileMsg) (File.toString file))

        LoadGP2 graph ->
            shouldLayout False (GP2Parser.parse graph) model

        LoadDot layoutDone graph ->
            shouldLayout layoutDone (parseGraph layoutDone graph) model

        SaveGP2 ->
            ( model, Download.string "graph.host" "text/plain" (GP2Graph.toGP2 model.editorModel.graph))

        SaveDot ->
            ( model, Download.string "graph.dot" "text/vnd.graphviz" (GP2Graph.toDot model.editorModel.graph))

        DismissError ->
            ( { model | error = False }, Cmd.none )


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
                setGraph layoutDone dotGraph model

            else
                ( model, Ports.Editor.layoutGraph (Encode.string (DotLang.toString dotGraph)))

        Err _ ->
            ( { model | error = True }, Cmd.none )


setGraph : Bool -> Dot -> Model -> ( Model, Cmd Msg )
setGraph shouldStretch graph model =
    let
        ( editorModel, cmd ) =
            GraphEditor.setGraph shouldStretch graph model.editorModel
    in
    ({ model | editorModel = editorModel, elementModel = ElementEditor.update editorModel (ElementEditor.Select GraphEditor.NullSelection) model.elementModel }, Cmd.map EditorMsg cmd)
            

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ GraphEditor.subscriptions model.editorModel
            |> Sub.map EditorMsg
        , Ports.Editor.layoutDone
            (Decode.decodeValue (Decode.string) >> Result.map (LoadDot True >> FileMsg) >> Result.withDefault (EditorMsg GraphEditor.NullMsg))
        ]


updateEditor : String -> String -> GraphEditor.Msg -> Model -> ( Model, Cmd Msg )
updateEditor nextNodeId nextEdgeId msg model =
    let
        ( newEditor, cmd ) =
            GraphEditor.update nextNodeId nextEdgeId msg model.editorModel
    in
    ( { model | editorModel = newEditor }, Cmd.map EditorMsg cmd )


updateElement : ElementEditor.ElementMsg -> Model -> Model
updateElement msg model =
    { model | elementModel = ElementEditor.update model.editorModel msg model.elementModel }
