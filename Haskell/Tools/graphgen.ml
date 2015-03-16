type mode  = Grid of int * int | Linear of int | Cyclic of int | SeriesParallel of int
type label = Empty | Int of int
type graph = Node of int * label | Edge of int * int * int * label
type settings = { mutable random_edge_labels:bool ; mutable random_node_labels:bool ; mutable tangle_factor:int }

open Printf


let nodeCount = ref 0
let edgeCount = ref 0

let nextNode ?(label=Empty) () =
    let nid = !nodeCount in
    nodeCount := nid + 1;
    Node (nid, label)
;;

let nextEdge ?(label=Empty) n1 n2 =
    if max n1 n2 >= !nodeCount then
        failwith "Tried to create an edge between undefined nodes"
    else begin
        let eid = ! edgeCount in
        edgeCount := eid + 1;
        Edge (eid, n1, n2, label)
    end
;;

let rec nodes ?(acc=[]) = function
    | 0 -> List.rev acc
    | n -> nodes ~acc:(nextNode ()::acc) (n-1)
;;

let strOfLabel = function
    | Empty -> "empty"
    | Int i -> string_of_int i
;;

let strOfGraph = function
    | Node (n,l)      -> sprintf "    (n%d, %s)" n (strOfLabel l)
    | Edge (id,n,m,l) -> sprintf "    (e%d, n%d, n%d, %s)" id n m (strOfLabel l)
;;

let rec linearEdges ?(acc=[]) = function
    | Node (n1, _) :: (Node (n2, _) :: _ as ns) ->
            linearEdges ~acc:(nextEdge n1 n2 :: acc) ns
    | _ -> List.rev acc
;;

let cyclicEdges ns = 
    let es = linearEdges ns in
    let join = nextEdge (List.length ns - 1) 0 in
    es @ [join]
;;

let linear n =
    let ns = nodes n in
    let es = linearEdges ns in
    (ns, es)
;;

let cyclic n =
    let ns = nodes n in
    let es = cyclicEdges ns in
    (ns, es)
;;

let rec riffle ?(acc=[]) r1 r2 = match (r1, r2) with
    | ([], []) -> List.rev acc
    | (Node (n1,_) :: xs, Node (n2,_) :: ys) ->
            riffle ~acc:(nextEdge n1 n2 :: acc) xs ys
    | _ -> failwith "non-rectangular grid! How did that happen?!"
;;

let rec cols ?(acc=[]) = function
    | row1 :: (row2 :: _ as rs) -> cols ~acc:(acc @ riffle row1 row2) rs
    | _ -> acc
;;

let grid x y =
    let row = linear in
    let rec rows ?(acc=[]) = function 
        | 0 -> List.rev acc
        | r -> rows ~acc:(row x :: acc) (r-1)
    in
    let (nss, ess) = List.split ( rows y ) in
    let ves = cols nss in
    (List.flatten nss, List.flatten ess @ ves)
;;

let parallel (ns, es) = match es with
    | Edge (e, s, t, lab) :: es ->
            (ns, nextEdge s t :: Edge (e, s, t, lab) :: es)
    | [] ->
            let ns = [nextNode (); nextNode ()] in
            let es = [nextEdge 1 0 ; nextEdge 1 0] in
            ( ns, es )
    | _ ->
            failwith "Nodes in the edge-list!"
;;

let settings = {
    random_edge_labels = false ;
    random_node_labels = false ;
    tangle_factor = 2 ;
}

let series (ns, es) = 
    match es with
    | Edge (e, s, t, lab) :: es' ->
            let Node (s', _) as n = nextNode () in
            begin match Random.int settings.tangle_factor with
            | 0 -> 
                (n :: ns, nextEdge s' s :: Edge (e, s, t, lab) :: es')
            | _ -> 
                (n :: ns, nextEdge s s' :: Edge (e, s', t, lab) :: es')
            end
    | [] ->
            let ns = [nextNode (); nextNode ()] in
            let es = [nextEdge 1 0] in
            (ns, es)
    | _ -> failwith "Edges in the node list!"
;;

let rec seriesParallel g = function
    | 0 -> g
    | n -> begin
        match Random.int 2 with
        | 0 -> seriesParallel (parallel g) (n-1)
        | _ -> seriesParallel (series g) (n-1)
    end
;;

let printGraph (ns, es) =
    let lines = List.flatten [["["]
        ; List.map strOfGraph ns
        ; ["|"]
        ; List.map strOfGraph es
        ; ["]"]
    ] in
    List.iter print_endline lines
;;

let randomIntLabel bound =
    Int ( Random.int bound )
;;

let randomLabel bound = function
    | Node (n, Empty)       -> Node (n, randomIntLabel bound)
    | Edge (e, s, t, Empty) -> Edge (e, s, t, randomIntLabel bound)
    | g  -> g
;;

let rec parseArgs = function
    | "-re" :: args -> settings.random_edge_labels <- true ; parseArgs args
    | "-rn" :: args -> settings.random_node_labels <- true ; parseArgs args
    | "-t"  :: v :: args -> settings.tangle_factor <- int_of_string v ; parseArgs args
    | "grid" :: x :: y :: [] ->
            Grid (int_of_string x, int_of_string y)
    | "linear" :: l :: [] ->
            Linear (int_of_string l)
    | "cyclic" :: c :: [] ->
            Cyclic (int_of_string c)
    | "sp" :: c :: [] ->
            SeriesParallel (int_of_string c)
    | _ -> failwith "You're doing it all wrong"
;;

let applyFilters (ns, es) =
    let nf = if settings.random_node_labels then randomLabel 10 else (fun x -> x) in
    let ef = if settings.random_edge_labels then randomLabel 10 else (fun x -> x) in
    (List.map nf ns, List.map ef es)
;;

let main () =
    Random.self_init () ;
    let mode = parseArgs (List.tl (Array.to_list Sys.argv)) in
    let graph = match mode with
    | Grid (x,y) ->
            grid x y
    | Linear l ->
            linear l
    | Cyclic c ->
            cyclic c
    | SeriesParallel n ->
            seriesParallel ([], []) n
    in
    printGraph (applyFilters graph)
;;

main ()
