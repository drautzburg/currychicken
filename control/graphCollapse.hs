import Data.List
import Data.List.Split
import Data.Maybe
import System.Process
import Control.Monad.State
import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Graphviz
import qualified Data.Graph.Inductive.PatriciaTree as P


type Id = Node
type Label = String

data CNode pl = N0 Label pl | NN Label [LNode (CNode pl)]  deriving (Eq)
instance Show (CNode pl)
        where 
            show (N0 lbl _)    = lbl
            show (NN lbl ns) = lbl ++":"++ (show ns) 

data CEdge = E0 Label | EN Label [LEdge CEdge]  deriving (Eq)
instance Show CEdge 
        where 
            show (E0 s) = "" -- s
            show (EN s _) = "["++s++"]"

------------------------------------------------------------
-- Creating simple Nodes and Eges
------------------------------------------------------------

n0 :: (Show pl) => Id -> pl -> (LNode (CNode pl))
n0 i pl = (i, N0 lbl pl)
             where
                 lbl = show pl ++ (show i)

nnEmpty :: Id -> Label -> (LNode (CNode pl))
nnEmpty i lbl = (i, NN lbl [])

e0 :: Id -> Id -> (LEdge CEdge)
e0 i j = (i,j, E0 lbl)
        where
            lbl = show i ++ "->" ++ show j

enEmpty :: Id -> Id -> Label -> (LEdge CEdge)
enEmpty i j lbl = (i,j, EN lbl [])

getPayload :: (CNode pl) -> pl
getPayload (N0 lbl payload)    = payload

getNodePayload :: LNode (CNode pl) -> pl
getNodePayload (id, n) = getPayload n

------------------------------------------------------------
-- Unique IDs
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

data Payload = Pl {
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


instance Show Payload where
        show pl = concatMap ( $ pl) [show . pType] 

exGraph :: Int -> State Int (P.Gr (CNode Payload) CEdge)
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

aggNodes :: Id -> Label -> [LNode (CNode pl)] -> LNode (CNode pl)
aggNodes i lbl nodes = foldr addTo (nnEmpty i lbl) nodes
        where
            addTo :: LNode (CNode pl) -> LNode (CNode pl) -> LNode (CNode pl)
            addTo nx (iAgg, (NN lblAgg nsAgg)) = (iAgg, (NN lblAgg (nx:nsAgg)))


disNNode :: LNode (CNode pl) -> [LNode (CNode pl)]
disNNode  (i, NN lbl nodes) = nodes
disNNode  (i, N0 lbl _) = error $ lbl ++ " is not an aggregate node."

aggEdges :: Id -> Id -> Label -> [LEdge CEdge] -> LEdge CEdge
aggEdges i j lbl edges = foldr addTo (enEmpty i j lbl) edges
        where
            addTo :: LEdge CEdge -> LEdge CEdge -> LEdge CEdge
            addTo ex (iAgg, jAgg, (EN lblAgg esAgg)) = (iAgg, jAgg, (EN lblAgg (ex:esAgg)))

disNEdge :: LEdge CEdge -> [LEdge CEdge]
disNEdge (i,j, EN lbl edges) = edges
disNEdge (i,j, E0 lbl) = error $ lbl ++ " is not an aggregate edge."


------------------------------------------------------------
-- Aggregator functions
------------------------------------------------------------

type Aggregator pl = (P.Gr (CNode pl) CEdge) -> Id -> Id -> Maybe pl

hasCommonNeighb  :: (Id -> [Node]) -> Aggregator pl

hasCommonNeighb f gr id1 id2 = 
        let succs =  map (fromJust . lab gr) $ intersect (f id1) (f id2)
        in case length succs of
               0 -> Nothing
               _ -> (Just . getPayload . head) succs -- use first common as new paload

hasCommonSucc :: Aggregator pl
hasCommonSucc gr  = hasCommonNeighb (suc gr) gr 

hasCommonPred :: Aggregator pl
hasCommonPred gr = hasCommonNeighb (pre gr) gr 


---getPayload $ head $ (map lab gr) $ intersect (suc gr id1) (suc gr id2)


{-

payload gr id = let (Just l) = lab gr id
                    Nothing  = error $ "Node " ++ (show id) ++ " is not in graph."
                in getPayload (id,l)

exPayP (a1,b1,c1) (a2,b2,c2) = a1 == a2

groupNodes :: (P.Gr CNode CEdge) -> Aggregator -> [[LNode CNode]]
groupNodes = undefined
-}


------------------------------------------------------------
-- Drawing
------------------------------------------------------------
draw graph = do
    let g = fst $ runState graph 0
    writeFile "xxx.dot" (graphviz' g)
    createProcess $ shell "dot -T eps -Gnodesep=0.1  -Nstyle=filled,rounded -Nshape=box -Nfontname=Arial -Efontname=Arial -Epenwidth=3 xxx.dot > xxx.eps"

