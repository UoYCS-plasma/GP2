module Syntax where

import GPSyntax

data Exp =  Seqn [Exp]
          | Alap Exp
          | If  Exp Exp         (Maybe Exp)
          | Try Exp (Maybe Exp) (Maybe Exp)
          | Or Exp Exp
          | Skip
          | Fail
    deriving (Show, Eq)

type Config = (Exp, GPHostGraph)

