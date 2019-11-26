module Editor.Element exposing (init, update, view, Model, Msg(..), ElementMsg(..), FileMsg(..))

import Editor.Graph as GraphEditor
import Graph
import GP2Graph.GP2Graph as GP2Graph
import Html exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import File exposing (File)
import DotLang exposing (Dot)
import GP2Graph.HostListParser as HostListParser
import GP2Graph.RuleListParser as RuleListParser
import Set exposing (Set)


type alias Model =
    { label : String
    , id : String
    , selection : GraphEditor.Selection
    }


type Msg
    = ElementMsg ElementMsg
    | EditorMsg GraphEditor.Msg
    | FileMsg FileMsg


type FileMsg
    = OpenGP2
    | OpenDot
    | SelectGP2 File
    | SelectDot File
    | LoadDot Bool String
    | LoadGP2 String
    | DismissError
    | SaveGP2
    | SaveDot


type ElementMsg
    = SetLabel String
    | SetId String
    | Select GraphEditor.Selection


init : Model
init =
    { label = blankLabel.label
    , id = blankLabel.id
    , selection = GraphEditor.NullSelection
    }


update : GraphEditor.Model -> ElementMsg -> Model -> Model
update editor msg model =
    case msg of
        Select selection ->
            { selection = selection
            , label = (getData selection editor.graph).label
            , id = (getData selection editor.graph).id
            }

        SetLabel label ->
            { model | label = label }

        SetId id ->
            { model | id = id }


isValid : String -> Bool
isValid label =
    case HostListParser.parse label of
        Ok _ ->
            True

        Err _ ->
            False


isValidR : String -> Bool
isValidR label =
    case RuleListParser.parse label of
        Ok _ ->
            True

        Err _ ->
            False


isValidId : Set String -> String -> Bool
isValidId ids id =
    case RuleListParser.parseId id of
        Ok _ ->
            not (Set.member id ids)

        Err _ ->
            False


validClass : (String -> Bool) -> String -> String
validClass validator label =
    if validator label then
        ""

    else
        "is-invalid"


getData : GraphEditor.Selection -> GP2Graph.VisualGraph -> GP2Graph.Label
getData selection graph =
    case selection of
        GraphEditor.NodeSelection id ->
            GP2Graph.getNodeData id graph
                |> Maybe.withDefault blankLabel

        GraphEditor.EdgeSelection from to id ->
            GP2Graph.getEdgeData from to id graph
                |> Maybe.withDefault blankLabel

        GraphEditor.NullSelection ->
            blankLabel


stringToMark : String -> GP2Graph.Mark
stringToMark str =
    case str of
        "none" ->
            GP2Graph.None

        "red" ->
            GP2Graph.Red

        "green" ->
            GP2Graph.Green

        "blue" ->
            GP2Graph.Blue

        "grey" ->
            GP2Graph.Grey

        "dashed" ->
            GP2Graph.Dashed

        "any" ->
            GP2Graph.Any

        _ ->
            GP2Graph.None

blankLabel =
    { label = ""
    , mark = GP2Graph.None
    , flag = False
    , id = ""
    }


markDecoder : GraphEditor.Selection -> Decoder Msg
markDecoder selection =
    Decode.field "value" Decode.string
        |> Decode.field "target"
        |> Decode.map (stringToMark >> GraphEditor.UpdateMark selection >> EditorMsg)


updateLabelDecoder : Bool -> GraphEditor.Selection -> String -> Decoder Msg
updateLabelDecoder host selection label =
    if (if host then isValid else isValidR) label then
        Decode.succeed (EditorMsg (GraphEditor.UpdateLabel selection label))

    else
        Decode.succeed (EditorMsg (GraphEditor.NullMsg))


updateIdDecoder : Set String -> GraphEditor.Selection -> String -> Decoder Msg
updateIdDecoder ids selection id =
    if isValidId ids id then
        Decode.succeed (EditorMsg (GraphEditor.UpdateId selection id))

    else
        Decode.succeed (EditorMsg (GraphEditor.NullMsg))


enterDecoder : Msg -> Decoder Msg
enterDecoder msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail "Non-enter key pressed"
            )


view : GraphEditor.Model -> GP2Graph.VisualGraph -> Model -> Html Msg
view editor otherGraph model =
    case editor.selection of
        GraphEditor.NodeSelection _ ->
            Html.form
                [ class "border-bottom form-inline m-0" ]
                (options editor.host True False editor.graph otherGraph model)

        GraphEditor.EdgeSelection _ _ _ ->
            Html.form
                [ class "border-bottom form-inline m-0" ]
                (options editor.host False False editor.graph otherGraph model)

        GraphEditor.NullSelection ->
            Html.form
                [ class "border-bottom form-inline m-0" ]
                (options editor.host False True editor.graph otherGraph init)


options : Bool -> Bool -> Bool -> GP2Graph.VisualGraph -> GP2Graph.VisualGraph -> Model -> List (Html Msg)
options host isNode disable graph otherGraph model =
    let
        data =
            getData model.selection graph

        nodeIds =
            (Graph.nodes graph)++(Graph.nodes otherGraph)
                |> List.map (.label >> Tuple.first >> .id)
                |> Set.fromList
                |> Set.remove data.id

        edgeIds =
            Graph.edges graph
                |> List.concatMap (.label >> List.map .id)
                |> Set.fromList
                |> Set.remove data.id

        idInput =
            div
                [ class "input-group my-2 ml-2"
                , hidden host
                ]
                [ div
                    [ class "input-group-prepend" ]
                    [ div [ class "input-group-text" ] [ text "ID:" ] ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , value model.id
                    , disabled disable
                    , onInput (SetId >> ElementMsg)
                    , preventDefaultOn
                            "keydown"
                            (updateIdDecoder (if isNode then nodeIds else edgeIds) model.selection model.id
                                |> Decode.andThen enterDecoder
                                |> Decode.andThen (\val -> Decode.succeed (val, True))
                            )
                    , class (validClass (\i -> isValidId (if isNode then nodeIds else edgeIds) i || disable) model.id)
                    ]
                    []
                , div
                    [ class "input-group-append" ]
                    [ input
                        [ type_ "button"
                        , class "input-group-text"
                        , disabled (disable || not (isValidId (if isNode then nodeIds else edgeIds) model.id))
                        , value "↲"
                        , on "click" (updateIdDecoder (if isNode then nodeIds else edgeIds) model.selection model.id)
                        ]
                        []
                    ]
                ]

        labelInput =
            div
                [ class "input-group m-2 " ]
                [ div
                    [ class "input-group-prepend" ]
                    [ div [ class "input-group-text" ] [ text "Label:" ] ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , value model.label
                    , disabled disable
                    , onInput (SetLabel >> ElementMsg)
                    , preventDefaultOn
                            "keydown"
                            (updateLabelDecoder host model.selection model.label
                                |> Decode.andThen enterDecoder
                                |> Decode.andThen (\val -> Decode.succeed (val, True))
                            )
                    , class (validClass (if host then isValid else isValidR) model.label)
                    ]
                    []
                , div
                    [ class "input-group-append" ]
                    [ input
                        [ type_ "button"
                        , class "input-group-text"
                        , disabled (disable || not (if host then isValid model.label else isValidR model.label))
                        , value "↲"
                        , on "click" (updateLabelDecoder host model.selection model.label)
                        ]
                        []
                    ]
                ]

        markInput =
            div
                [ class "input-group mr-2 my-2" ]
                [ div
                    [ class "input-group-prepend" ]
                    [ div [ class "input-group-text" ] [ text "Mark:" ] ]
                , select
                    [ on "change" (markDecoder model.selection)
                    , class "form-control"
                    , disabled disable
                    ]
                    [ option
                        [ selected (data.mark == GP2Graph.None)
                        , value "none"
                        ]
                        [ text "None" ]
                    , option
                        [ selected (data.mark == GP2Graph.Red)
                        , value "red"
                        ]
                        [ text "Red" ]
                    , option
                        [ selected (data.mark == GP2Graph.Green)
                        , value "green"
                        ]
                        [ text "Green" ]
                    , option
                        [ selected (data.mark == GP2Graph.Blue)
                        , value "blue"
                        ]
                        [ text "Blue" ]
                    , option
                        [ selected (data.mark == GP2Graph.Grey)
                        , value "grey"
                        , hidden (not isNode)
                        ]
                        [ text "Grey" ]
                    , option
                        [ selected (data.mark == GP2Graph.Dashed)
                        , value "dashed"
                        , hidden isNode
                        ]
                        [ text "Dashed" ]
                    , option
                        [ selected (data.mark == GP2Graph.Any)
                        , value "any"
                        , hidden host
                        ]
                        [ text "Any" ]
                    ]
                ]

        flagInput =
            div
                [ hidden ((not isNode) && host || disable)
                , class "input-group my-2 mr-sm-2"
                ]
                [ div
                    [ class "input-group-prepend" ]
                    [ div
                        [ class "input-group-text" ]
                        [ text <|
                            if isNode then
                                "Root:"

                            else
                                "Bidirectional:"
                        ]
                    ]
                , div
                    [ class "input-group-append" ]
                    [ div
                        [ class "input-group-text" ]
                        [ input
                            [ type_ "checkbox"
                            , disabled disable
                            , checked data.flag
                            , onCheck (GraphEditor.UpdateFlag model.selection >> EditorMsg)
                            ]
                            []
                        ]
                    ]
                ]

        open =
            div
                [ class "my-2 mr-sm-2 ml-auto btn-group dropdown" ]
                [ input
                    [ type_ "button"
                    , class "btn btn-outline-primary"
                    , value "Open"
                    , onClick (FileMsg OpenDot)
                    ]
                    []
                , button
                    [ class "btn btn-outline-primary dropdown-toggle dropdown-toggle-split", attribute "data-toggle" "dropdown" ]
                    []
                , div
                    [ class "dropdown-menu dropdown-menu-right" ]
                    [ a [ class "dropdown-item", href "#", onClick (FileMsg OpenDot) ] [ text "DOT" ]
                    , a [ class "dropdown-item", href "#", onClick (FileMsg OpenGP2) ] [ text "GP2" ]
                    ]
                ]

        save =
            div
                [ class "my-2 mr-sm-2 btn-group dropdown" ]
                [ input
                    [ type_ "button"
                    , class "btn btn-primary"
                    , value "Save"
                    , onClick (FileMsg SaveDot)
                    ]
                    []
                , button
                    [ class "btn btn-primary dropdown-toggle dropdown-toggle-split", attribute "data-toggle" "dropdown" ]
                    []
                , div
                    [ class "dropdown-menu dropdown-menu-right" ]
                    [ a [ class "dropdown-item", href "#", onClick (FileMsg SaveDot) ] [ text "DOT" ]
                    , a [ class "dropdown-item", href "#", onClick (FileMsg SaveGP2) ] [ text "GP2" ]
                    ]
                ]

    in
    [ idInput, labelInput, markInput, flagInput, open, save ]
