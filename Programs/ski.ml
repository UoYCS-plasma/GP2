
type ski = S of (ski*ski*ski) option
         | K of (ski*ski) option
         | I of ski option
;;

let nodeId = ref 0
let nextNodeId () = let n = nodeId in nodeId <- n+1; n ;;

let edgeId = ref 0
let nextEdgeId () = let e = edgeId in edgeId <- e+1; e ;;

let makeNode t =
    ( Printf.sprintf "\t(n%d, '%s')\n" (nextNodeId ()) t , "" )
;;
let makeApp t args = 
 

let rec showSki = function
    | S None -> makeNode "S"
    | K None -> makeNode "K"
    | I None -> makeNode "I"
    | S (Some a, b, c) -> makeApp "S" [showSki a; showSki b; showSki c]
    | K (Some a, b)    -> makeApp "K" [showSki a; showSki b]
    | I (Some a)       -> makeApp "I" [showSki a]
;;


