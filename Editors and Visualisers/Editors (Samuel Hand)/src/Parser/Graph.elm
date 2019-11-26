module Parser.Graph exposing (Edge(..), Graph(..), IntExpr(..), Label(..), ListExpr(..), Mark(..), Node(..), StringExpr(..), toString)


type Graph
    = Graph (List Node) (List Edge)


type Node
    = Node String Label


type Edge
    = Edge String String String Label


type Label
    = Label ListExpr Mark


type Mark
    = None
    | Red
    | Green
    | Blue
    | Grey
    | Dashed
    | Any


type ListExpr
    = LVar String
    | Empty
    | Int IntExpr
    | String StringExpr
    | LCons ListExpr ListExpr


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


toString : Graph -> String
toString = Debug.toString
