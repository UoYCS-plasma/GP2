module GP2Graph.HostListParser exposing (hostListParser, parse)

import Basics.Extra exposing (flip)
import Parser exposing (..)
import GP2Graph.GP2Graph as GP2Graph


parse : String -> Result (List DeadEnd) GP2Graph.HostList
parse list =
    run (hostListParser |. end) list


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')


hostListParser : Parser GP2Graph.HostList
hostListParser =
    sequence
        { start = ""
        , end = ""
        , spaces = whitespace
        , separator = ":"
        , item = hostListItem
        , trailing = Forbidden
        }


hostListItem : Parser GP2Graph.HostListItem
hostListItem =
    oneOf
        [ succeed GP2Graph.Empty |. empty
        , succeed GP2Graph.HostInt |= negInt
        , succeed GP2Graph.HostString |= string
        ]


empty : Parser ()
empty =
    keyword "empty"


negInt : Parser Int
negInt =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]


string : Parser String
string =
    succeed identity
        |. symbol "\""
        |= (getChompedString <| chompUntil "\"")
        |. symbol "\""
