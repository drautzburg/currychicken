import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import System.Process
import Control.Monad.State
import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Graphviz
import qualified Data.Graph.Inductive.PatriciaTree as P


type Label = String

data CNode pl = N0 Label pl | NN Label [LNode (CNode pl)]  deriving (Eq)
instance Show (CNode pl)
        where 
            show (N0 lbl _)    = lbl
            show (NN lbl ns) = lbl -- ++":"++ (show ns) 

data CEdge = E0 Label | EN Label [LEdge CEdge]  deriving (Eq)
instance Show CEdge 
        where 
            show (E0 s) = "" -- s
            show (EN s _) = "["++s++"]"

type CGraph a = P.Gr (CNode a) CEdge
------------------------------------------------------------
-- Creating simple Nodes and Eges
------------------------------------------------------------

-- | Node is actually just an Id (i.e. an Int)
n0 :: (Show pl) => Node -> pl -> (LNode (CNode pl))
n0 i pl = (i, N0 lbl pl)
             where
                 lbl = show pl ++ (show i)

nnEmpty :: Node -> Label -> (LNode (CNode pl))
nnEmpty i lbl = (i, NN lbl [])

e0 :: Node -> Node -> (LEdge CEdge)
e0 i j = (i,j, E0 lbl)
        where
            lbl = show i ++ "->" ++ show j

enEmpty :: Node -> Node -> Label -> (LEdge CEdge)
enEmpty i j lbl = (i,j, EN lbl [])

getPayload :: (CNode pl) -> pl
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
    return  $ (n0 id payload)


createNodes :: (Show pl) => [pl] -> State Int [LNode (CNode pl)]
createNodes payloads = sequence $ map createNode payloads

connect :: [(LNode (CNode pl),LNode (CNode pl))] -> [LEdge CEdge]
connect nodePairs = map newEdge nodePairs

newEdge ((i,_), (j,_)) = e0 i j

type NodeEq pl = LNode (CNode pl) -> LNode (CNode pl) -> Bool

-- | Connect all nodes in nodes which satisfy an equality criterion
connectAll :: [LNode (CNode pl)] -> (NodeEq pl)-> [LEdge CEdge]
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

splitIn :: Int -> [a] -> [[a]]
splitIn n xs 
             | n == 0       = []
             | otherwise    = (take piece xs) : splitIn (n-1) (drop piece xs)
             where
                 len   = length xs
                 piece =  ceiling $ (fromIntegral len)/ (fromIntegral n)


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
        edges = concat [
                 connectAll nodes (\x y -> and [
                                            origDest x y,
                                            prodAcc x y 
                                           ])
                ]

    return $ mkGraph nodes edges
          where
              named :: Int -> String -> [String]
              named n s = [s ++ (show x) | x <- [1..n]]
              distribute xs ys = zip xs (splitIn (length xs) ys)

              intersects s1 s2 = intersect s1 s2 /= []
              origDest x y     = (dest (getNodePayload x)) == (orig (getNodePayload y))
              prodAcc x y      = (produces (getNodePayload x)) `intersects`  (accepts (getNodePayload y))

------------------------------------------------------------
-- aggregating and dissecting individual nodes and edges
------------------------------------------------------------

aggNodes :: (Node,Label) -> [LNode (CNode pl)] -> LNode (CNode pl)
aggNodes (i,lbl) nodes = foldr addTo (nnEmpty i lbl) nodes
        where
            addTo :: LNode (CNode pl) -> LNode (CNode pl) -> LNode (CNode pl)
            addTo nx (iAgg, (NN lblAgg nsAgg)) = (iAgg, (NN lblAgg (nx:nsAgg)))


disNNode :: LNode (CNode pl) -> [LNode (CNode pl)]
disNNode  (i, NN lbl nodes) = nodes
disNNode  (i, N0 lbl _) = error $ lbl ++ " is not an aggregate node."

aggEdges :: (Node,Node,Label) -> [LEdge CEdge] -> LEdge CEdge
aggEdges (i,j,lbl) edges = foldr addTo (enEmpty i j lbl) edges
        where
            addTo :: LEdge CEdge -> LEdge CEdge -> LEdge CEdge
            addTo ex (iAgg, jAgg, (EN lblAgg esAgg)) = (iAgg, jAgg, (EN lblAgg (ex:esAgg)))

disNEdge :: LEdge CEdge -> [LEdge CEdge]
disNEdge (i,j, EN lbl edges) = edges
disNEdge (i,j, E0 lbl) = error $ lbl ++ " is not an aggregate edge."


------------------------------------------------------------
-- Aggregator functions
------------------------------------------------------------

type Aggregator pl = (CGraph pl) -> Node -> Node -> Maybe pl

hasCommonNeighb  :: (Node -> [Node]) -> Aggregator pl

hasCommonNeighb f gr id1 id2 = 
        let succs =  map (fromJust . lab gr) $ intersect (f id1) (f id2)
        in case length succs of
               0 -> Nothing
               _ -> (Just . getPayload . head) succs -- use first common as new paload

hasCommonSucc :: Aggregator pl
hasCommonSucc gr  = hasCommonNeighb (suc gr) gr 

hasCommonPred :: Aggregator pl
hasCommonPred gr = hasCommonNeighb (pre gr) gr 

------------------------------------------------------------
-- Collapsing and expanding graphs
------------------------------------------------------------



id2node :: Graph gr => gr t b -> Node -> LNode t
id2node g n = (n, fromJust $ lab g n)

xgroup :: (Eq a, Ord b) => [(a,b)] -> (a->b->Bool) -> [([a],b)]
xgroup l f = let unq = (nub . sort . map snd) l
          in do
              u <- unq
              let xs = [fst x | x <- l, snd x == u]
              return (xs, u)

-- aggEdges :: (Node,Node,Label) -> [LEdge CEdge]      -> LEdge CEdge

{-
Several Problems:
(1) It is too difficult to read, particularly the Edges stuff
(2) I see new edges [(2,23,[bar]),(3,23,[bar]),(6,23,[bar]),(8,23,[bar])]
    Where 23 is the new node. Nodes 2 and 3 were collapsed and are no longer in the tree => exception
    Edges which are completely inside the group do not have a collapsed edge, they should just disappear
    This however, means that the whole idea of "EN" Edges may be wrong
    However, if we want to uncollapse we need the old edges somewhere.
(3) I am getting doubts that the NN and EN types are such a good idea. Alternatively one might hold the 
    original graph and the collapsed graph. Problem is how to uncollapse part of the graph.  Still it might 
    be possible to operate only on the original graph and produce collapsed graphs as needed. Uncollapsing part
    of the graph would then be just another (weaker) collapse of the original graph. We would then need an algebra on
    operations, such that we can compute collapse -> uncollapse -> collapse
(4) I have doubt about the order of (from,to) in the new edges
-}

groupNodes :: Label -> [Node] -> State Int (CGraph pl) -> State Int (CGraph pl)
groupNodes label ids graph =  do
    gr <- graph
    id <- nextval 
    let oldNodes = map (id2node gr) ids
        newNode  = (aggNodes (id,label) oldNodes) 
        gr1      = insNode newNode gr 
        gr2      = foldr delNode' gr1  oldNodes
        -- Now the edges:
        fromEdgesMapping = do
            old   <- ids
            toOld <- from $ context gr old            :: [(CEdge, Node)]
            let oldEdge = (snd toOld, old, fst toOld) :: LEdge CEdge
                newEdge = (snd toOld, id, "bar")      :: (Node, Node, Label)
            return (oldEdge, newEdge)

        newEdgesTo = map (\(olds,new) -> aggEdges new olds) $ xgroup fromEdgesMapping (\(a,_,_)(b,_,_) -> a==b) 
        gr3 = foldr insEdge gr2  (trace (show newEdgesTo) newEdgesTo)

    return $ gr3
            where
                delNode' (i,_) g = delNode i g
                to (toNode,n,l,f) = toNode
                from (a,b,c,d) = d
                sameNode (_,i) (_,j) = i==j




exGraph2 = groupNodes "foo"  [0,1,2,3,4] (exGraph 1)
        

unState  gr = fst $ runState gr 0
    


------------------------------------------------------------
-- Drawing
------------------------------------------------------------
draw graph = do
    let g = unState graph
    writeFile "xxx.dot" (graphviz' g)
    createProcess $ shell "dot -T eps -Gnodesep=0.1  -Nstyle=filled,rounded -Nshape=box -Nfontname=Arial -Efontname=Arial -Epenwidth=3 xxx.dot > xxx.eps"

