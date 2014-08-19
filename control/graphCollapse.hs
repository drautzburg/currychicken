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

data CNode pl = N0 Label pl | NN Label [LNode (CNode pl)]  deriving (Eq)
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
        walks             = (10*fan) `named` "WK"
        delOffices        = (6*fan)  `named` "DO"

    inUsort  <- createNodes [pl 'T'  ["enters"] ["From"++collOffice] collOffice "fromColl"
                            | collOffice <- collOffices
                            ]
    unpUsort <- createNodes [pl 'U'  ["From"++collOffice] ["unsorted"] "fromColl" "OUTW"
                            | collOffice <- collOffices
                            ]
    inPsort  <- createNodes [pl 'T'  [center] ["From"++center] center "fromCenters"
                            | center <- centers
                            ]
    unpPsort <- createNodes [pl 'U'  ["From"++center] myInwGroups "fromCenters" "fromCenters"
                            | center <- centers
                            ]
    arr2_inw <- createNodes [pl 't' [inwGroup] [inwGroup] "fromCenters" "INW"
                            | inwGroup <- myInwGroups
                            ]
    srtOutw  <- createNodes [pl 'O'  ["unsorted"] (myInwGroups++theirInwGroups) "OUTW" "OUTW"
                            | x<-outwRuns
                            ]
    outw_inw <- createNodes [pl 't'  [inwGroup] [inwGroup] "OUTW" "INW"
                            | inwGroup <- myInwGroups
                            ]
    srtInw   <- createNodes [pl 'I'  [inwardGroup]  seqGroup "INW" "SEQ"
                            |(inwardGroup, seqGroup) <- distribute myInwGroups sequenceRuns
                            ]
    srtSeq   <- createNodes [pl 'S'  [sequenceRun] walk "SEQ" "DEPART1"
                            | (sequenceRun, walk) <- distribute sequenceRuns walks
                            ]
    outSeq   <- createNodes [pl 'T'  walk ["For"++delOffice] "DEPART1" delOffice
                            | (delOffice, walk) <- distribute delOffices walks
                            ]
    outPsort <- createNodes [pl 'T'  inwardGroup ["For"++center] "OUTW" center
                            | (center, inwardGroup) <- distribute centers theirInwGroups
                            ]

    let nodes = inUsort ++ unpUsort  ++ inPsort ++ unpPsort ++ srtOutw ++ outPsort ++ srtInw ++ srtSeq ++ outSeq ++ outw_inw ++ arr2_inw
        edges = connectAll nodes (\x y -> origDest x y
                                          &&
                                          prodAcc x y 
                                 )


    return $ mkGraph nodes edges
          where
              named :: Int -> String -> [String]
              named n s = [s ++ show x | x <- [1..n]]
              distribute xs ys = zip xs (splitIn (length xs) ys)

              intersects s1 s2 = intersect s1 s2 /= []
              origDest x y     = dest (getNodePayload x) == orig (getNodePayload y)
              prodAcc x y      = produces (getNodePayload x) `intersects`  accepts (getNodePayload y)

------------------------------------------------------------
-- aggregating and dissecting individual nodes and edges
------------------------------------------------------------

aggNodes :: (Node,Label) -> [LNode (CNode pl)] -> LNode (CNode pl)
aggNodes (i,lbl) = foldr addTo (nnEmpty i lbl) 
        where
            addTo :: LNode (CNode pl) -> LNode (CNode pl) -> LNode (CNode pl)
            addTo nx (iAgg, NN lblAgg nsAgg) = (iAgg, NN lblAgg (nx:nsAgg))


disNNode :: LNode (CNode pl) -> [LNode (CNode pl)]
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


samePayload (_, N0 _ pl1) (_, N0 _ pl2) = pl1 == pl2

aggregateBy :: (Eq pl, Ord pl) => CGraph pl -> Aggregator pl -> [(Label, [Node])]
aggregateBy gr aggf = map lblNodes $ M.toList $ M.fromListWith (++) triplets
  where
    triplets = [((lbl, pl),[n]) | (n, N0 lbl pl) <- map aggf $ labNodes gr]
    lblNodes :: ((Label, pl),[Node]) -> (Label, [Node])
    lblNodes ((lbl,pl),ns) = (lbl, ns)


------------------------------------------------------------
-- Collapsing and expanding graphs
------------------------------------------------------------


-- Helpers
id2node :: Graph gr => gr t b -> Node -> LNode t
id2node g n = (n, fromJust $ lab g n)


-- aggEdges :: (Node,Node,Label) -> [LEdge CEdge]      -> LEdge CEdge


type GraphTransform pl = CGraph pl -> CGraph pl


groupNodes :: State Int (CGraph pl) -> Label -> [Node] ->  State Int (CGraph pl)
groupNodes graph label ids  =  do
    gr    <- graph
    newId <- nextval 
    let oldNodes    = map (id2node gr) (trc "ids" ids)
        newNode     = aggNodes (newId,label) oldNodes
        addNewNode  = insNode $ trc "newNode" newNode  
        delOldNodes = delNodes $ trc "delNode" ids     :: GraphTransform pl
        -- Now the edges:
        oldEdgesTo = do
            old     <- ids
            toOld   <- pre gr old
            return (toOld, old)
        oldEdgesFrom = do
            old     <- ids
            fromOld   <- suc gr old
            return $ (old, fromOld)
        oldEdgesWithin = trc "EdgesWithin" [(i,j) | (i,j,lbl) <- labEdges gr, i `elem` ids, j `elem` ids]
        newEdges    = trc "newEdges" $ uniq $ reconnect 2 newId  (oldEdgesTo   -/ oldEdgesWithin) ++ 
                                              reconnect 1 newId  (oldEdgesFrom -/ oldEdgesWithin)
        delOldEdges = delEdges $ trc "delEdge" (oldEdgesTo ++ oldEdgesFrom)  :: GraphTransform pl
        -- beware: inserting edge between nodes wich do not exists causes "Irrefutable pattern failed"
        addNewEdges = insEdges $ map (uncurry e0) newEdges                :: GraphTransform pl
        
    return $ (addNewEdges . delOldEdges . addNewNode . delOldNodes) gr
            where
                to (toNode,n,l,f) = toNode
                from (a,b,c,d) = d
                uniq = nub . sort
                (-/) = O.minus
                reconnect :: Int -> Int -> [Edge] -> [Edge] 
                reconnect pos id edges  = case pos of
                  1 -> map (\(i, j) -> (id, j)) edges
                  2 -> map (\(i, j) -> (i, id)) edges


exGraph2 :: State Int (CGraph ExPayload)
exGraph2 = groupNodes (exGraph 1) "foo"  [0,1,2,3,4,12,13,14,15] 


exGraph3 :: State Int (CGraph ExPayload)
exGraph3 = groupNodes' theGraph nodeGroups
  where
    -- xxx there must be a way to avoid this explicit recursion
    theGraph          = (exGraph 4)
    groupNodes' gr [] = gr
    groupNodes' gr ((lbl, nodes):groups) = groupNodes' (groupNodes gr lbl nodes) groups
    nodeGroups        = (aggregateBy g1 byFrom)
    g1                = evalState0 theGraph


evalState0  gr = evalState gr 0
    


------------------------------------------------------------
-- Drawing
------------------------------------------------------------
draw graph = do
    let g = evalState0 graph
    writeFile "xxx.dot" (graphviz' g)
    createProcess $ shell "dot -T eps -Gnodesep=0.1  -Nstyle=filled,rounded -Nshape=box -Nfontname=Arial -Efontname=Arial -Epenwidth=3 xxx.dot > xxx.eps"

