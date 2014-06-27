module SemanticAnalysis where

import Prelude hiding (lookup)
import Data.List  
import GPSyntax
import ExAr
import Graph

testProg :: GPProgram
testProg = Program 
   [MainDecl (Main 
      (Sequence 
        [Block (SimpleCommand (RuleCall "rule1")),
         Block (SimpleCommand (RuleCall "rule2"))]
      )
    ),
   RuleDecl (AstRule "rule1" 
       [("i",IntVar),("a",AtomVar)] 
       (AstRuleGraph 
          [RuleNode "n1" True (RuleLabel [] Uncoloured),
           RuleNode "n2" False (RuleLabel [Var ("i",ListVar),Var ("a",ListVar)] Uncoloured)] 
          [RuleEdge False "n1" "n2" (RuleLabel [Val (Str "abc")] Uncoloured)],
       AstRuleGraph
          [RuleNode "n1" False (RuleLabel [Var ("b",ListVar)] Uncoloured),
           RuleNode "n2" True (RuleLabel [Val (Str "q")] Uncoloured)] 
          [RuleEdge False "n1" "n2" (RuleLabel [Var ("a",ListVar)] Uncoloured)])
       ["n1","n2"] 
       (And (Eq [Indeg "n1"] [Val (Int 2)]) (Greater (Var ("i",ListVar)) (Val (Int 5)))) "true"
    ),
   ProcDecl (Procedure "proc1" 
       [RuleDecl (AstRule "rule1"
        [("a",AtomVar),("a",AtomVar)]
        (AstRuleGraph [] [], AstRuleGraph [] [])
        ["n5","n5"]
        (And (TestInt "a") (NEq ([Var ("a",ListVar)]) ([Var ("x",ListVar)])))
        "true")
       ]
       (Sequence [Block (SimpleCommand (RuleCall "rule3"))])
    )
   ]




