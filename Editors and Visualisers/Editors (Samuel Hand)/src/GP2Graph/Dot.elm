module GP2Graph.Dot exposing (needsLayout)


import DotLang exposing (Attr(..), Dot(..), EdgeRHS(..), EdgeType(..), ID(..), Stmt(..))


needsLayout : Dot -> Bool
needsLayout (Dot _ _ stmts) =
    List.any (placedNode >> not) stmts


placedNode : Stmt -> Bool
placedNode stmt =
    case stmt of
        NodeStmt _ attrs ->
            List.any isPos attrs

        _ ->
            True


isPos : Attr -> Bool
isPos (Attr id _) =
    id == ID "pos"
