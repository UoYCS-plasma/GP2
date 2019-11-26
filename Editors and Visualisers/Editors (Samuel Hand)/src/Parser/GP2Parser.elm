module Parser.GP2Parser exposing (parseGraph, parseProgram, intExpr)

import Basics.Extra exposing (flip)
import Parser.Graph exposing (..)
import Parser exposing (..)
import Parser.Program as Program exposing (..)
import Set


parseProgram =
    (Result.mapError deadEndsToString) << (run program)


parseGraph =
    (Result.mapError deadEndsToString) << (run hostGraph)



{-
   - Some general "helper" parsers
-}


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')


lexeme : Parser a -> Parser a
lexeme =
    flip (|.) whitespace


var : Parser String
var =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '_', reserved = Set.fromList [ "edge", "indeg", "outdeg", "length", "empty", "atom", "int", "string", "char", "not", "and", "or", "where", "interface", "if", "then", "else", "try", "skip", "fail", "break" ] }


procName : Parser String
procName =
    variable { start = Char.isUpper, inner = \c -> Char.isAlphaNum c || c == '_', reserved = Set.fromList [ "Main" ] }


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


reverseDivAssoc divExpr =
    let
        helper e acc =
            case e of
                Div intA intB ->
                    helper intB (Div (acc intA))

                _ ->
                    acc e
    in
    helper divExpr identity



{-
   - Parse int expressions
-}


intExpr : Bool -> Parser IntExpr
intExpr isRule =
    binOpAdditive isRule


iTerm : Bool -> Parser IntExpr
iTerm isRule =
    oneOf
        ((if isRule then
            [ iVar ]

          else
            []
         )
            ++ [ iLiteral, iParen isRule, neg isRule, indeg, outdeg, length ]
        )


iVar : Parser IntExpr
iVar =
    lexeme <| succeed IVar |= var


iLiteral : Parser IntExpr
iLiteral =
    lexeme <| succeed ILiteral |= int


iParen : Bool -> Parser IntExpr
iParen isRule =
    succeed identity |. (lexeme << symbol) "(" |= lazy (\_ -> intExpr isRule) |. (lexeme << symbol) ")"


neg : Bool -> Parser IntExpr
neg isRule =
    succeed Neg |. symbol "-" |= lazy (\_ -> intExpr isRule)


indeg : Parser IntExpr
indeg =
    succeed Indeg |. (lexeme << keyword) "indeg" |. (lexeme << symbol) "(" |= lexeme var |. (lexeme << symbol) ")"


outdeg : Parser IntExpr
outdeg =
    succeed Outdeg |. (lexeme << keyword) "outdeg" |. (lexeme << symbol) "(" |= lexeme var |. (lexeme << symbol) ")"


length : Parser IntExpr
length =
    succeed Length |. (lexeme << keyword) "length" |. (lexeme << symbol) "(" |= lexeme var |. (lexeme << symbol) ")"


binOpMultiplicative : Bool -> Parser IntExpr
binOpMultiplicative isRule =
    lexeme (iTerm isRule) |> andThen (\t -> oneOf [ mul isRule t, div isRule t, succeed t ])


mul : Bool -> IntExpr -> Parser IntExpr
mul isRule t =
    succeed (Mul t) |. (lexeme << symbol) "*" |= lazy (\_ -> binOpMultiplicative isRule)


div : Bool -> IntExpr -> Parser IntExpr
div isRule t =
    succeed (Div t) |. (lexeme << symbol) "/" |= lazy (\_ -> binOpMultiplicative isRule) |> andThen (succeed << reverseDivAssoc)


binOpAdditive : Bool -> Parser IntExpr
binOpAdditive isRule =
    lexeme (binOpMultiplicative isRule) |> andThen (\t -> oneOf [ add isRule t, sub isRule t, succeed t ])


add : Bool -> IntExpr -> Parser IntExpr
add isRule t =
    succeed (Add t) |. (lexeme << symbol) "+" |= lazy (\_ -> binOpAdditive isRule)


sub : Bool -> IntExpr -> Parser IntExpr
sub isRule t =
    succeed (Sub t) |. (lexeme << symbol) "-" |= lazy (\_ -> binOpAdditive isRule) |> andThen (succeed << reverseSubAssoc)



{-
   - Parse string expressions
-}


stringExpr : Bool -> Parser StringExpr
stringExpr isRule =
    oneOf [ sCons isRule, sTerm isRule ]


sTerm : Bool -> Parser StringExpr
sTerm isRule =
    oneOf
        ((if isRule then
            [ sVar ]

          else
            []
         )
            ++ [ sLiteral ]
        )


sLiteral : Parser StringExpr
sLiteral =
    succeed SLiteral |. symbol "\"" |= (getChompedString <| (chompWhile <| flip (/=) '"')) |. (lexeme << symbol) "\""


sVar : Parser StringExpr
sVar =
    lexeme <| succeed SVar |= var


sCons : Bool -> Parser StringExpr
sCons isRule =
    lexeme (sTerm isRule) |> andThen (\t -> oneOf [ succeed (SCons t) |. (lexeme << symbol) "." |= lazy (\_ -> stringExpr isRule), succeed t ])



{-
   - Parse list expressions
-}


listExpr : Bool -> Parser ListExpr
listExpr isRule =
    oneOf [ lCons isRule, lTerm isRule ]


lTerm : Bool -> Parser ListExpr
lTerm isRule =
    oneOf
        ((if isRule then
            [ lVar ]

          else
            []
         )
            ++ [ empty, succeed Int |= intExpr isRule, succeed String |= stringExpr isRule ]
        )


lVar : Parser ListExpr
lVar =
    lexeme <| succeed LVar |= var


empty : Parser ListExpr
empty =
    lexeme <| succeed Empty |. keyword "empty"


lCons : Bool -> Parser ListExpr
lCons isRule =
    lexeme (lTerm isRule) |> andThen (\t -> oneOf [ succeed (LCons t) |. (lexeme << symbol) ":" |= lazy (\_ -> listExpr isRule), succeed t ])



{-
   - Parse conditions
-}


condition : Parser Condition
condition =
    or


cTerm : Parser Condition
cTerm =
    oneOf [ cParen, edge, varType, intIneq, listIneq ]


cParen : Parser Condition
cParen =
    succeed identity |. (lexeme << symbol) "(" |= lazy (\_ -> condition) |. (lexeme << symbol) ")"


intIneq : Parser Condition
intIneq =
    intExpr True |> andThen (\t -> oneOf [ oneOf [ lt t, le t, gt t, ge t ], succeed (Int t) |> andThen (\u -> oneOf [ eq u, ne u ]) ])


lt : IntExpr -> Parser Condition
lt t =
    succeed (Program.LT t) |. (lexeme << symbol) "<" |= intExpr True


le : IntExpr -> Parser Condition
le t =
    succeed (LE t) |. (lexeme << symbol) "<=" |= intExpr True


gt : IntExpr -> Parser Condition
gt t =
    succeed (Program.GT t) |. (lexeme << symbol) ">" |= intExpr True


ge : IntExpr -> Parser Condition
ge t =
    succeed (GE t) |. (lexeme << symbol) ">=" |= intExpr True


listIneq : Parser Condition
listIneq =
    listExpr True |> andThen (\t -> oneOf [ eq t, ne t ])


eq : ListExpr -> Parser Condition
eq t =
    succeed (Program.EQ t) |. (lexeme << symbol) "=" |= listExpr True


ne : ListExpr -> Parser Condition
ne t =
    succeed (NE t) |. (lexeme << symbol) "!=" |= listExpr True


label : Bool -> Parser Label
label isRule =
    succeed Label |= listExpr isRule |= oneOf [ succeed identity |. (lexeme << symbol) "#" |= mark, succeed None ]


edge : Parser Condition
edge =
    succeed EdgePred |. (lexeme << keyword) "edge" |. (lexeme << symbol) "(" |= lexeme var |. (lexeme << symbol) "," |= lexeme var |= oneOf [ succeed Just |. (lexeme << symbol) "," |= lexeme (label True), succeed Nothing ] |. (lexeme << symbol) ")"


mark : Parser Mark
mark =
    oneOf [ succeed Any |. (lexeme << keyword) "any", succeed Red |. (lexeme << keyword) "red", succeed Green |. (lexeme << keyword) "green", succeed Blue |. (lexeme << keyword) "blue", succeed Dashed |. (lexeme << keyword) "dashed" ]


varType : Parser Condition
varType =
    succeed VarType |= oneOf [ succeed AtomT |. (lexeme << keyword) "atom", succeed IntT |. (lexeme << keyword) "int", succeed StringT |. (lexeme << keyword) "string", succeed CharT |. (lexeme << keyword) "char" ] |. (lexeme << symbol) "(" |= lexeme var |. (lexeme << symbol) ")"


notC : Parser Condition
notC =
    succeed Not |. (lexeme << keyword) "not" |= lazy (\_ -> cTerm)


and : Parser Condition
and =
    lexeme (oneOf [ notC, cTerm ]) |> andThen (\t -> oneOf [ succeed (And t) |. (lexeme << keyword) "and" |= lazy (\_ -> and), succeed t ])


or : Parser Condition
or =
    lexeme and |> andThen (\t -> oneOf [ succeed (Or t) |. (lexeme << keyword) "or" |= lazy (\_ -> or), succeed t ])



{-
   - Parse expressions
-}


expr : Parser Expr
expr =
    seq


eTerm : Parser Expr
eTerm =
    oneOf [ eParen, ruleSet, procCall, skip, fail, break ]


eParen : Parser Expr
eParen =
    succeed identity |. (lexeme << symbol) "(" |= lazy (\_ -> eTerm) |. (lexeme << symbol) ")"


procCall : Parser Expr
procCall =
    succeed ProcCall |= lexeme procName


skip : Parser Expr
skip =
    succeed Skip |. (lexeme << keyword) "skip"


fail : Parser Expr
fail =
    succeed Fail |. (lexeme << keyword) "fail"


break : Parser Expr
break =
    succeed Break |. (lexeme << keyword) "break"


ruleSet : Parser Expr
ruleSet =
    succeed RuleSet |= oneOf [ succeed List.singleton |= var, sequence { start = "{", separator = ",", end = "}", spaces = whitespace, item = var, trailing = Forbidden } ]


conditional : Parser Expr
conditional =
    oneOf [ eIf, try, loop ]


eIf : Parser Expr
eIf =
    succeed If |. (lexeme << keyword) "if" |= lazy (\_ -> expr) |. (lexeme << keyword) "then" |= lazy (\_ -> expr) |= oneOf [ succeed identity |. (lexeme << keyword) "else" |= lazy (\_ -> expr), succeed Skip ]


try : Parser Expr
try =
    succeed Try |. (lexeme << keyword) "try" |= lazy (\_ -> expr) |= oneOf [ succeed identity |. (lexeme << keyword) "then" |= lazy (\_ -> expr), succeed Skip ] |= oneOf [ succeed identity |. (lexeme << keyword) "else" |= lazy (\_ -> expr), succeed Skip ]


loop : Parser Expr
loop =
    eTerm |> andThen (\t -> oneOf [ succeed (Program.Loop t) |. (lexeme << symbol) "!", succeed t ])


eOr : Parser Expr
eOr =
    conditional |> andThen (\t -> oneOf [ succeed (EOr t) |. (lexeme << keyword) "or" |= lazy (\_ -> eOr), succeed t ])


seq : Parser Expr
seq =
    eOr |> andThen (\t -> oneOf [ succeed (Seq t) |. (lexeme << symbol) ";" |= lazy (\_ -> seq), succeed t ])



{-
   - Parse declarations
-}


decl : Parser Decl
decl =
    oneOf [ proc, rule ]


rule : Parser Decl
rule =
    succeed Rule |= lexeme var |= lexeme typeList |= lexeme ruleGraph |. (lexeme << symbol) "=>" |= lexeme ruleGraph |. interface |= maybeCondition


typeDecl : Parser VarType
typeDecl =
    oneOf [ succeed ListT |. (lexeme << keyword) "list", succeed AtomT |. (lexeme << keyword) "atom", succeed IntT |. (lexeme << keyword) "int", succeed StringT |. (lexeme << keyword) "string", succeed CharT |. (lexeme << keyword) "char" ]


typeList : Parser (List Var)
typeList =
    sequence
        { start = "("
        , separator = ";"
        , end = ")"
        , spaces = whitespace
        , item = succeed Var |= varList |. (lexeme << symbol) ":" |= lexeme typeDecl
        , trailing = Forbidden
        }


varList : Parser (List String)
varList =
    sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = whitespace
        , item = var
        , trailing = Forbidden
        }


ruleGraph : Parser Graph
ruleGraph =
    succeed Graph |= sequence { start = "[", separator = "", end = "|", spaces = whitespace, item = ruleNode, trailing = Forbidden } |= sequence { start = "", separator = "", end = "]", spaces = whitespace, item = ruleEdge, trailing = Forbidden }


hostGraph : Parser Graph
hostGraph =
    succeed Graph |= sequence { start = "[", separator = "", end = "|", spaces = whitespace, item = hostNode, trailing = Forbidden } |= sequence { start = "", separator = "", end = "]", spaces = whitespace, item = hostEdge, trailing = Forbidden }


hostNode : Parser Node
hostNode =
    succeed Node |. (lexeme << symbol) "(" |= lexeme var |. (lexeme << symbol) "," |= lexeme (label False) |. (lexeme << symbol) ")"


hostEdge : Parser Edge
hostEdge =
    succeed Edge |. (lexeme << symbol) "(" |= lexeme var |. (lexeme << symbol) "," |= lexeme var |. (lexeme << symbol) "," |= lexeme var |. (lexeme << symbol) "," |= lexeme (label False) |. (lexeme << symbol) ")"


ruleNode : Parser Node
ruleNode =
    succeed Node |. (lexeme << symbol) "(" |= lexeme var |. (lexeme << symbol) "," |= lexeme (label True) |. (lexeme << symbol) ")"


ruleEdge : Parser Edge
ruleEdge =
    succeed Edge |. (lexeme << symbol) "(" |= lexeme var |. (lexeme << symbol) "," |= lexeme var |. (lexeme << symbol) "," |= lexeme var |. (lexeme << symbol) "," |= lexeme (label True) |. (lexeme << symbol) ")"


interface : Parser ()
interface =
    (lexeme << keyword) "interface" |. (lexeme << symbol) "=" |. (lexeme << symbol) "{" |. varList |. (lexeme << symbol) "}"


maybeCondition : Parser (Maybe Condition)
maybeCondition =
    oneOf [ succeed Just |. (lexeme << keyword) "where" |= condition, succeed Nothing ]


proc : Parser Decl
proc =
    succeed Proc |= lexeme procName |. (lexeme << symbol) "=" |= oneOf [ lexeme declList, succeed [] ] |= expr


declList : Parser (List Decl)
declList =
    sequence
        { start = "["
        , separator = ""
        , end = "]"
        , spaces = whitespace
        , item = lazy (\_ -> decl)
        , trailing = Forbidden
        }


tDecl : Parser TDecl
tDecl =
    oneOf [ mainProc, succeed Decl |= decl ]


mainProc : Parser TDecl
mainProc =
    succeed Main |. (lexeme << keyword) "Main" |. (lexeme << symbol) "=" |= expr



{-
   - Parse the entire program - a list of declarations
-}


program : Parser Program.Program
program =
    succeed Program.Program |= sequence { start = "", separator = "", end = "", spaces = whitespace, item = tDecl, trailing = Forbidden } |. end
