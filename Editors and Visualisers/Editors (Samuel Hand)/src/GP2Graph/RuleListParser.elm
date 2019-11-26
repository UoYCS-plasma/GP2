module GP2Graph.RuleListParser exposing (parse, listExpr, id, parseId)


import Parser exposing (..)
import GP2Graph.GP2Graph as GP2Graph exposing (RuleList, RuleListItem(..), StringExpr(..), IntExpr(..))
import Set


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')


lexeme : Parser a -> Parser a
lexeme parser =
    parser |. whitespace


var : Parser String
var =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList
            [ "edge"
            , "indeg"
            , "outdeg"
            , "length"
            , "empty"
            , "int"
            , "string"
            , "char"
            , "not"
            , "and"
            , "or"
            , "where"
            , "interface"
            , "if"
            , "then"
            , "else"
            , "try"
            , "skip"
            , "fail"
            , "break"
            ]
        }


id : Parser String
id =
    oneOf
        [ var
        , int
            |> map String.fromInt
        ]


reverseSubAssoc : IntExpr -> IntExpr
reverseSubAssoc subExpr =
    let
        helper e acc =
            case e of
                Sub intA intB ->
                    helper intB (Sub (acc intA))

                _ ->
                    acc e
    in
    helper subExpr identity


reverseDivAssoc : IntExpr -> IntExpr
reverseDivAssoc divExpr =
    let
        helper e acc =
            case e of
                Div intA intB ->
                    helper intB (Div (acc intB))

                _ ->
                    acc e
    in
    helper divExpr identity


parse : String -> Result (List DeadEnd) RuleList
parse list =
    run (listExpr |. end) list


parseId : String -> Result (List DeadEnd) String
parseId s =
    run (id |. end) s


listExpr : Parser RuleList
listExpr =
    sequence
        { start = ""
        , separator = ":"
        , end = ""
        , spaces = whitespace
        , item = listItem
        , trailing = Forbidden
        }


listItem : Parser RuleListItem
listItem =
    oneOf
        [ succeed RuleEmpty
            |. (keyword >> lexeme) "empty"
        , lexeme var
            |> andThen  (\t -> oneOf [ succeed StringExpr |= stringExprS t, succeed IntExpr |= intExprS t, succeed (LVar t) ])
        , succeed StringExpr
            |= stringExpr
        , succeed IntExpr
            |= intExpr True
        ]


stringExpr : Parser StringExpr
stringExpr =
    stringItem
        |> andThen (\t -> oneOf [ cons t, succeed t ])

cons : StringExpr -> Parser StringExpr
cons t =
    succeed (SCons t)
        |. (symbol >> lexeme) "."
        |= stringExpr


stringItem : Parser StringExpr
stringItem =
    oneOf
        [ succeed SLiteral
            |. symbol "\""
            |= (chompWhile (\c -> c /= '"')
                    |> getChompedString)
            |. (symbol >> lexeme) "\""
        , succeed SVar
            |= lexeme var
        ]


stringExprS : String -> Parser StringExpr
stringExprS s =
    cons (SVar s)


intExprS : String -> Parser IntExpr
intExprS s =
    oneOf [ add (IVar s), sub (IVar s), mul (IVar s), div (IVar s) ]
        |> map reverseSubAssoc
        |> map reverseDivAssoc


intExpr : Bool -> Parser IntExpr
intExpr reverse =
    lexeme (mulExpr True)
        |> andThen (\t -> oneOf [ add t, sub t, succeed t ])
        |> map (if reverse then reverseSubAssoc else identity)


mulExpr : Bool -> Parser IntExpr
mulExpr reverse =
    lexeme iTerm
        |> andThen (\t -> oneOf [ mul t, div t, succeed t ])
        |> map (if reverse then reverseDivAssoc else identity)


iTerm : Parser IntExpr
iTerm =
    oneOf [ iParen, neg, indeg, outdeg, length, iLiteral, iVar ]


add : IntExpr -> Parser IntExpr
add t =
    succeed (Add t)
        |. (symbol >> lexeme) "+"
        |= lazy (\_ -> intExpr True)


sub : IntExpr -> Parser IntExpr
sub t =
    succeed (Sub t)
        |. (symbol >> lexeme) "-"
        |= lazy (\_ -> intExpr False)


mul : IntExpr -> Parser IntExpr
mul t =
    succeed (Mul t)
        |. (symbol >> lexeme) "*"
        |= lazy (\_ -> mulExpr True)

div : IntExpr -> Parser IntExpr
div t =
    succeed (Div t)
        |. (symbol >> lexeme) "/"
        |= lazy (\_ -> mulExpr False)


iParen : Parser IntExpr
iParen =
    succeed identity
        |. (symbol >> lexeme) "("
        |= lazy (\_ -> intExpr True)
        |. (symbol >> lexeme) ")"


neg : Parser IntExpr
neg =
    succeed Neg
        |. symbol "-"
        |= lazy (\_ -> iTerm)


indeg : Parser IntExpr
indeg =
    succeed Indeg
        |. (symbol >> lexeme) "indeg"
        |. (symbol >> lexeme) "("
        |= lexeme id
        |. (symbol >> lexeme) ")"


outdeg : Parser IntExpr
outdeg =
    succeed Outdeg
        |. (symbol >> lexeme) "outdeg"
        |. (symbol >> lexeme) "("
        |= lexeme id
        |. (symbol >> lexeme) ")"


length : Parser IntExpr
length =
    succeed Length
        |. (symbol >> lexeme) "length"
        |. (symbol >> lexeme) "("
        |= lexeme var
        |. (symbol >> lexeme) ")"


iLiteral : Parser IntExpr
iLiteral =
    succeed ILiteral
        |= lexeme int


iVar : Parser IntExpr
iVar =
    succeed IVar
        |= lexeme var
