import Data.List
import Data.Function
import qualified Data.Map as M
import qualified Data.List.Ordered as O
import Data.List.Split
import Data.Maybe
import Debug.Trace
import System.Process
import Control.Monad.State
import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Graphviz
import qualified Data.Graph.Inductive.PatriciaTree as P

trc s x = trace (s ++ ":" ++show x) x

type Label = String

-- in the NN case we only need NN Label [Node] and no payload
data CNode pl = N0 Label pl | NN Label [Node]  deriving (Eq)
instance Show (CNode pl)
        where 
            show (N0 lbl _)    = lbl
            show (NN lbl ns) = lbl -- ++":"++ (show ns) 

data CEdge = E0 Label deriving (Eq)
instance Show CEdge 
        where 
            show (E0 s) = "" -- s

type CGraph a = P.Gr (CNode a) CEdge
------------------------------------------------------------
-- Creating simple Nodes and Eges
------------------------------------------------------------

-- | Node is actually just an Id (i.e. an Int)
n0 :: (Show pl) => Node -> pl -> LNode (CNode pl)
n0 i pl = (i, N0 lbl pl)
             where
                 lbl = show pl ++ show i

nnEmpty :: Node -> Label -> LNode (CNode pl)
nnEmpty i lbl = (i, NN lbl [])

e0 :: Node -> Node -> LEdge CEdge
e0 i j = (i,j, E0 lbl)
        where
            lbl = show i ++ "->" ++ show j

getPayload :: CNode pl -> pl
getPayload (N0 lbl payload)    = payload

getNodePayload :: LNode (CNode pl) -> pl
getNodePayload (id, n) = getPayload n

------------------------------------------------------------
-- Unique Node IDs
------------------------------------------------------------

nextval :: State Int Int
nextval = do
    id <- get
    put (id+1)
    return id


createNode :: (Show pl) => pl -> State Int (LNode (CNode pl))
createNode payload = do
    id <- nextval
    return  $ n0 id payload


createNodes :: (Show pl) => [pl] -> State Int [LNode (CNode pl)]
createNodes = mapM createNode

connect :: [(LNode (CNode pl),LNode (CNode pl))] -> [LEdge CEdge]
connect = map newEdge 

newEdge ((i,_), (j,_)) = e0 i j

type NodeEq pl = LNode (CNode pl) -> LNode (CNode pl) -> Bool

-- | Connect all nodes in nodes which satisfy an equality criterion
connectAll :: [LNode (CNode pl)] -> NodeEq pl-> [LEdge CEdge]
connectAll nodes eqf = [newEdge (from, to)| from<-nodes, to<-nodes, eqf from to]


------------------------------------------------------------
-- Example Graph
------------------------------------------------------------

data ExPayload = Pl {
            pType    :: Char, 
            accepts  :: [String], 
            produces :: [String], 
            orig     :: String,
            dest     :: String
} deriving (Eq, Ord)

pl t a p o d = Pl {
                   pType    = t,
                   accepts  = a,
                   produces = p,
                   orig     = o,
                   dest     = d
               }

nullPayload = pl '-' [] [] "" ""

splitIn :: Int -> [a] -> [[a]]
splitIn n xs 
             | n == 0       = []
             | otherwise    = take piece xs : splitIn (n-1) (drop piece xs)
             where
                 len   = length xs
                 piece =  ceiling $ (fromIntegral len ::Double)/ fromIntegral n


instance Show ExPayload where
        show pl = concatMap ( $ pl) [show . pType] 

exGraph :: Int -> State Int (CGraph ExPayload)
exGraph fan = do   
    let collOffices       = (2*fan)  `named` "CO"
        centers           = (2*fan)  `named` "CE"
        myInwGroups       = (2*fan)  `named` "MIG"
        theirInwGroups    = (3*fan)  `named` "TIG"
        outwRuns          = fan      `named` "OR"
        sequenceRuns      = (4*fan)  `named` "SQ"
        walks             = (16*fan) `named` "WK"
        delOffices        = (6*fan)  `named` "DO"

    inUsort  <- createNodes [pl 'T'  ["enters"] ["From"++collOffice] collOffice "CollArrival"
                            | collOffice <- collOffices
                            ]
    unpUsort <- createNodes [pl 'U'  ["From"++collOffice] ["From"++collOffice++"Unp"] "CollArrival" "CollArrival"
                            | collOffice <- collOffices
                            ]
    collArr_outw <- createNodes [pl 't'  ["From"++collOffice++"Unp"] ["From"++collOffice++"Unp"] "CollArrival" "OutwArea"
                            | collOffice <- collOffices
                            ]
    inPsort  <- createNodes [pl 'T'  [center] ["From"++center] center "IctArrival"
                            | center <- centers
                            ]
    unpPsort <- createNodes [pl 'U'  ["From"++center] myInwGroups "IctArrival" "IctArrival"
                            | center <- centers
                            ]
    ict_inw <- createNodes [pl 't' [inwGroup] [inwGroup] "IctArrival" "InwArea"
                            | inwGroup <- myInwGroups
                            ]
    srtOutw  <- createNodes [pl 'O'  ["From"++collOffice++"Unp"] (myInwGroups++theirInwGroups) "OutwArea" "OutwArea"
                            | x<-outwRuns, collOffice <- collOffices
                            ]
    outw_inw <- createNodes [pl 't'  [inwGroup] [inwGroup] "OutwArea" "InwArea"
                            | inwGroup <- myInwGroups
                            ]
    srtInw   <- createNodes [pl 'I'  [inwardGroup]  seqGroup "InwArea" "InwArea"
                            |(inwardGroup, seqGroup) <- distribute myInwGroups sequenceRuns
                            ]
    inw_seq   <- createNodes [pl 't'  [seqGroup]  [seqGroup] "InwArea" "SeqArea"
                            |seqGroup <- sequenceRuns
                            ]
    srtSeq   <- createNodes [pl 'S'  [sequenceRun] walk "SeqArea" "SeqArea"
                            | (sequenceRun, walk) <- distribute sequenceRuns walks
                            ]
    seq_dept <- createNodes [pl 't'  [walk] [walk] "SeqArea" "DoDeparture"
                            | walk <- walks
                            ]
    outSeq   <- createNodes [pl 'T'  walk ["For"++delOffice] "DoDeparture" delOffice
                            | (delOffice, walk) <- distribute delOffices walks
                            ]
    outw_ict <- createNodes [pl 't'  inwardGroup ["For"++center] "OutwArea" "IctDepart"
                            | (center, inwardGroup) <- distribute centers theirInwGroups
                            ]
    outPsort <- createNodes [pl 'T'  ["For"++center] ["For"++center] "IctDepart" center
                            | (center, inwardGroup) <- distribute centers theirInwGroups
                            ]

    let nodes = inUsort
                ++ unpUsort
                ++ collArr_outw
                ++ inPsort
                ++ unpPsort
                ++ srtOutw
                ++ outw_ict
                ++ outPsort
                ++ srtInw
                ++ srtSeq
                ++ outSeq
                ++ outw_inw
                ++ inw_seq
                ++ seq_dept
                ++ ict_inw
        edges = connectAll nodes (\x y -> origDest x y
                                          &&
                                          prodAcc x y 
                                 )


    return $ mkGraph nodes edges
          where
              named :: Int -> String -> [String]
              named n s = [s ++ show x | x <- [1..n]]
              -- associate groups of ys with xs
              distribute xs ys = zip xs (splitIn (length xs) ys)

              intersects s1 s2 = intersect s1 s2 /= []
              origDest x y     = dest (getNodePayload x) == orig (getNodePayload y)
              prodAcc x y      = produces (getNodePayload x) `intersects`  accepts (getNodePayload y)

------------------------------------------------------------
-- aggregating and dissecting individual nodes and edges
------------------------------------------------------------

aggNodes :: (Node,Label) -> [Node] -> LNode (CNode pl)
aggNodes (i,lbl) = foldr addTo (nnEmpty i lbl) 
        where
            addTo :: Node -> LNode (CNode pl) -> LNode (CNode pl)
            addTo nx (iAgg, NN lblAgg nsAgg) = (iAgg, NN lblAgg (nx:nsAgg))

disNNode :: LNode (CNode pl) -> [Node]
disNNode  (i, NN lbl nodes) = nodes
disNNode  (i, N0 lbl _) = error $ lbl ++ " is not an aggregate node."


------------------------------------------------------------
-- Aggregator functions
------------------------------------------------------------

type Aggregator pl = LNode (CNode pl) -> LNode (CNode pl)

byFrom :: Aggregator ExPayload
byFrom (n, N0 lbl pl) = (n, N0 lbl' pl')
        where
            pl'  = nullPayload {orig = orig pl}
            lbl' = "From " ++ orig pl

byFromTo :: Aggregator ExPayload
byFromTo (n, N0 lbl pl) = (n, N0 lbl' pl')
        where
            pl'  = nullPayload {orig = orig pl, dest = dest pl}
            lbl' = orig pl ++ "->" ++ dest pl

byFromToTyped :: Aggregator ExPayload
byFromToTyped (n, N0 lbl pl) = (n, N0 lbl' pl')
        where
            pl'  = nullPayload {orig = orig', dest = dest'}
            lbl' = [pType pl] ++ ":" ++ (if orig' == dest'
                                         then orig'
                                         else orig' ++ "->" ++ dest'
                                        )
            orig' = typed $ orig pl
            dest' = typed $ dest pl
            typed s = case take 2 s of
              "CE" -> "Centers"
              "CO" -> "Coll"
              "DO" -> "Delivery"
              _    -> s


samePayload (_, N0 _ pl1) (_, N0 _ pl2) = pl1 == pl2

aggregate :: (Eq pl, Ord pl) => CGraph pl -> Aggregator pl -> [(Label, [Node])]
aggregate gr aggf = map lblNodes $ M.toList $ M.fromListWith (++) triplets
  where
    triplets = [((lbl, pl),[n]) | (n, N0 lbl pl) <- map aggf $ labNodes gr]
    lblNodes :: ((Label, pl),[Node]) -> (Label, [Node])
    lblNodes ((lbl,pl),ns) = (lbl, ns)


------------------------------------------------------------
-- Collapsing and expanding graphs
------------------------------------------------------------


-- Helpers

uniq = nub . sort
a `without` b = O.minus (sort a)  (sort b)


type GraphTransform pl = CGraph pl -> CGraph pl


groupNodes :: State Int (CGraph pl) -> Label -> [Node] ->  State Int (CGraph pl)
groupNodes graph label ids  =  do
    gr    <- graph
    newId <- nextval 
    let newNode     = aggNodes (newId,label) ids
        addNewNode  = insNode newNode  
        delOldNodes = delNodes ids     :: GraphTransform pl
        -- Now the edges:
        oldEdgesTo = do
            old     <- ids
            toOld   <- pre gr old
            return (toOld, old)
        oldEdgesFrom = do
            old     <- ids
            fromOld   <- suc gr old
            return $ (old, fromOld)
        oldEdgesWithin = [(i,j) | (i,j,lbl) <- labEdges gr, i `elem` ids, j `elem` ids]
        newEdges    = uniq $ reconnect 2 newId  (oldEdgesTo   `without`  oldEdgesWithin) ++ 
                             reconnect 1 newId  (oldEdgesFrom  `without` oldEdgesWithin)
        delOldEdges = delEdges $ oldEdgesTo ++ oldEdgesFrom  :: GraphTransform pl
        -- beware: inserting edge between nodes wich do not exists causes "Irrefutable pattern failed"
        addNewEdges = insEdges $ map (uncurry e0) newEdges                :: GraphTransform pl
        
    return $ (addNewEdges . delOldEdges . addNewNode . delOldNodes) gr
            where
                to (toNode,n,l,f) = toNode
                from (a,b,c,d) = d
                reconnect :: Int -> Int -> [Edge] -> [Edge] 
                reconnect pos id edges  = case pos of
                  1 -> map (\(i, j) -> (id, j)) edges
                  2 -> map (\(i, j) -> (i, id)) edges


exGraph2 :: State Int (CGraph ExPayload)
exGraph2 = groupNodes (exGraph 1) "foo"  [0,1,2,3,4,12,13,14,15] 


-- xxx error: trying to insert edge between nodes which do not exist
exGraph3 :: State Int (CGraph ExPayload)
exGraph3 = groupNodes' theGraph nodeGroups
  where
    -- xxx there must be a way to avoid this explicit recursion
    theGraph          = (exGraph 4)
    groupNodes' gr [] = gr
    groupNodes' gr ((lbl, nodes):groups) = groupNodes' (groupNodes gr lbl nodes) groups
    nodeGroups        = aggregate g1 byFromToTyped
    g1                = evalState0 theGraph


evalState0  gr = evalState gr 0
    


------------------------------------------------------------
-- Drawing
------------------------------------------------------------
draw graph = do
    let g = evalState0 graph
        dot = "D:/Software/Graphviz/bin/dot"
    writeFile "xxx.dot" (graphviz' g)
    createProcess $ shell $ dot ++ " -T svg -Gnodesep=0.1  -Nstyle=filled,rounded -Nshape=box -Nfontname=Arial -Efontname=Arial -Epenwidth=3 xxx.dot > xxx.svg"

