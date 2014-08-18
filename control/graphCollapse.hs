import Data.List
import Data.Function
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
} deriving (Eq)

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
        theirInwGroups    = (2*fan)  `named` "TIG"
        outwRuns          = fan      `named` "OR"
        sequenceRuns      = (4*fan)  `named` "SQ"
        walks             = (10*fan) `named` "WK"
        delOffices        = (6*fan)  `named` "DO"

    inUsort  <- createNodes [pl 'T'  ["enters"] ["From"++collOffice] collOffice "ARR1"
                                     | collOffice <- collOffices
                            ]
    unpUsort <- createNodes [pl 'U'  ["From"++collOffice] ["unsorted"] "ARR1" "OUTW"
                                     | collOffice <- collOffices
                            ]
    inPsort  <- createNodes [pl 'T'  [center] ["From"++center] center "ARR2"
                                     | center <- centers
                            ]
    unpPsort <- createNodes [pl 'U'  ["From"++center] myInwGroups "ARR2" "INW"
                                     | center <- centers
                            ]
    srtOutw  <- createNodes [pl 'O'  ["unsorted"] (myInwGroups++theirInwGroups) "OUTW" "INW"
                                     |x<-outwRuns
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
    outPsort <- createNodes [pl 'T'  inwardGroup ["For"++center] "INW" center
                                     | (center, inwardGroup) <- distribute centers theirInwGroups
                            ]

    let nodes = inUsort ++ unpUsort  ++ inPsort ++ unpPsort ++ srtOutw ++ outPsort ++ srtInw ++ srtSeq ++ outSeq
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

samePayload (_, N0 _ pl1) (_, N0 _ pl2) = pl1 == pl2

aggregateBy
  :: (Eq a, Graph gr) =>
     gr a1 b1 -> (LNode a1 -> (b, CNode a)) -> [[b]]

aggregateBy gr agg =  map (map fst) $ groupBy samePayload $  map agg $ labNodes gr

-- xxx need to get the labels

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
    let oldNodes    = map (id2node gr) ids
        newNode     = aggNodes (newId,label) oldNodes
        addNewNode  = insNode $ trc "newn" newNode  
        delOldNodes = delNodes $ trc "deln" ids     :: GraphTransform pl
        -- Now the edges:
        oldEdgesTo = do
            old     <- ids
            toOld   <- pre gr old
            return $ trc "oet"(toOld, old)
        oldEdgesFrom = do
            old     <- ids
            fromOld   <- suc gr old
            return $ trc "oef" (old, fromOld)
        oldEdgesWithin = trc "oewi" [(i,j) | (i,j,lbl) <- labEdges gr, i `elem` ids, j `elem` ids]
        newEdges    = uniq $ remap 2 newId  oldEdgesTo   oldEdgesWithin ++ 
                             remap 1 newId  oldEdgesFrom oldEdgesWithin
        delOldEdges = delEdges $ trc "dele" (oldEdgesTo ++ oldEdgesFrom)  :: GraphTransform pl
        addNewEdges = insEdges $ map (uncurry e0) newEdges                :: GraphTransform pl
        
    return $ (addNewEdges . delOldEdges . addNewNode . delOldNodes) gr
            where
                to (toNode,n,l,f) = toNode
                from (a,b,c,d) = d
                uniq = nub . sort
                remap pos id edges edges' = let es = edges `O.minus` edges'
                                            in case pos of
                                                   1 -> map (\(i, j) -> (id, j)) es
                                                   2 -> map (\(i, j) -> (i, id)) es


exGraph2 = groupNodes (exGraph 1) "foo"  [0,1,2,3,4,12,13,14,15] 

-- xxx
exGraph3 = foldr (groupNodes (exGraph 1) "g3") (exGraph 1) (aggregateBy g1 byFrom)
           where 
               g1 = unState (exGraph 1)

unState  gr = evalState gr 0
    


------------------------------------------------------------
-- Drawing
------------------------------------------------------------
draw graph = do
    let g = unState graph
    writeFile "xxx.dot" (graphviz' g)
    createProcess $ shell "dot -T eps -Gnodesep=0.1  -Nstyle=filled,rounded -Nshape=box -Nfontname=Arial -Efontname=Arial -Epenwidth=3 xxx.dot > xxx.eps"

