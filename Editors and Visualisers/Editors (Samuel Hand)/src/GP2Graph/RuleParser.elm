module GP2Graph.RuleParser exposing (parse)


import Parser exposing (..)
import DotLang exposing (Dot(..), Stmt(..), ID(..), NodeId(..), AttrStmtType(..), EdgeRHS(..), EdgeType(..), Attr(..))
import GP2Graph.RuleListParser exposing (listExpr)
import GP2Graph.GP2Parser exposing (position, whitespace, lexeme, stmtList)
import GP2Graph.GP2Graph as GP2Graph


parse : String -> Result (List DeadEnd) Dot
parse graph =
    run ruleParser graph


ruleParser : Parser Dot
ruleParser =
    succeed (\v s c -> Dot Digraph (Just (ID (v++"="++c))) s)
        |= (chompWhile (\c -> c /= '[') |> getChompedString |> map String.trim)
        |= graphs
        |. whitespace
        |. (keyword >> lexeme) "interface"
        |. (symbol >> lexeme) "="
        |. (symbol >> lexeme) "{"
        |. chompWhile (\c -> c /= '}')
        |. (symbol >> lexeme) "}"
        |= oneOf [ succeed identity |. (keyword >> lexeme) "where" |= (chompUntilEndOr "\n" |> getChompedString), succeed "" ]
        |. whitespace


graphs : Parser (List Stmt)
graphs =
    succeed processGraphs
        |. whitespace
        |. (symbol >> lexeme) "["
        |. oneOf [ succeed () |. position |. (symbol >> lexeme) "|" , succeed () ]
        |= stmtList
        |. (symbol >> lexeme) "]"
        |. (symbol >> lexeme) "=>"
        |. (symbol >> lexeme) "["
        |. oneOf [ succeed () |. position |. (symbol >> lexeme) "|" , succeed () ]
        |= stmtList
        |. (symbol >> lexeme) "]"


processGraphs : List Stmt -> List Stmt -> List Stmt
processGraphs left right =
    (List.map (leftStmt right) left) ++ (List.filterMap (rightStmt left) right)



getNode : List Stmt -> String -> Maybe (List Attr)
getNode stmts id =
    case stmts of
        (NodeStmt (NodeId (ID node) _) attrs)::rest ->
            if node == id then
                Just attrs

            else
                getNode rest id

        _::rest ->
            getNode rest id

        [] ->
            Nothing

getEdge : List Stmt -> String -> Maybe (List Attr)
getEdge stmts id =
    case stmts of
        (EdgeStmtNode _ _ _ attrs)::rest ->
            if GP2Graph.getAttr "id" attrs |> Maybe.map ((==) id) |> Maybe.withDefault False then
                Just attrs

            else
                getEdge rest id

        _::rest ->
            getEdge rest id

        [] ->
            Nothing


addIdPrefix : String -> List Attr -> List Attr
addIdPrefix prefix attrs =
    case attrs of
        (Attr (ID "id") (ID id))::rest ->
            (Attr (ID "id") (ID (prefix++"_"++id)))::(addIdPrefix prefix rest)

        attr::rest ->
            attr::(addIdPrefix prefix rest)

        [] ->
            []


combineAttrs : List Attr -> List Attr -> List Attr
combineAttrs left right =
    case left of
        (Attr (ID "label") (ID label))::rest ->
            (Attr (ID "label") (ID (label++"/"++(GP2Graph.getAttr "label" right |> Maybe.withDefault ""))))::(combineAttrs rest right)

        (Attr (ID "fillcolor") (ID colour))::rest ->
            (Attr (ID "fillcolor") (ID (colour++";50:"++(GP2Graph.getAttr "fillcolor" right |> Maybe.withDefault "white"))))::(combineAttrs rest right)

        (Attr (ID "style") (ID style))::rest ->
            (Attr (ID "style") (ID (style++","++(GP2Graph.getAttr "style" right |> Maybe.map (String.split ",") |> Maybe.andThen List.tail |> Maybe.andThen List.head |> Maybe.withDefault "solid"))))::(combineAttrs rest right)

        attr::rest ->
            attr::(combineAttrs rest right)

        [] ->
            []


leftStmt : List Stmt -> Stmt -> Stmt
leftStmt right stmt =
    case stmt of
        NodeStmt (NodeId (ID id) _) attrs ->
            case getNode right id of
                Just otherAttrs ->
                    NodeStmt (NodeId (ID id) Nothing) ((Attr (ID "shape") (ID "ellipse"))::(combineAttrs attrs otherAttrs))

                Nothing ->
                    NodeStmt (NodeId (ID id) Nothing) ((Attr (ID "shape") (ID "rect"))::attrs)

        EdgeStmtNode from (EdgeNode to) _ attrs ->
            case GP2Graph.getAttr "id" attrs |> Maybe.andThen (getEdge right) of
                Just _ ->
                    EdgeStmtNode from (EdgeNode to) [] ((Attr (ID "arrowhead") (ID "obox"))::(Attr (ID "arrowtail") (ID "obox"))::(addIdPrefix "left" attrs))

                Nothing ->
                    EdgeStmtNode from (EdgeNode to) [] ((Attr (ID "arrowhead") (ID "obox"))::(Attr (ID "arrowtail") (ID "obox"))::attrs)

        _ ->
            stmt


rightStmt : List Stmt -> Stmt -> Maybe Stmt
rightStmt left stmt =
    case stmt of
        NodeStmt (NodeId (ID id) _) attrs ->
            case getNode left id of
                Just _ ->
                    Nothing

                Nothing ->
                    NodeStmt (NodeId (ID id) Nothing) ((Attr (ID "shape") (ID "diamond"))::attrs) |> Just

        EdgeStmtNode from (EdgeNode to) _ attrs ->
            case GP2Graph.getAttr "id" attrs |> Maybe.andThen (getEdge left) of
                Just _ ->
                    EdgeStmtNode from (EdgeNode to) [] ((Attr (ID "arrowhead") (ID "odiamond"))::(Attr (ID "arrowtail") (ID "odiamond"))::(addIdPrefix "right" attrs)) |> Just

                Nothing ->
                    EdgeStmtNode from (EdgeNode to) [] ((Attr (ID "arrowhead") (ID "odiamond"))::(Attr (ID "arrowtail") (ID "odiamond"))::attrs) |> Just

        _ ->
            Just stmt
