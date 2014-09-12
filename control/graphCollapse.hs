import Data.List
import GHC.Exts
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

data Payload a = PL {label :: String, payload::a}
               deriving (Eq)

instance Show (Payload a)
        where
            show = label



data CNode pl = N0 (Payload pl) | NN (Payload ())   deriving (Eq)
instance Show (CNode pl)
        where 
            show (N0 pl) = show pl
            show (NN pl) = show pl -- ++":"++ (show ns) 


data CEdge = E0 Label deriving (Eq)
instance Show CEdge 
        where 
            show (E0 s) = "" -- s


type CGraph a = P.Gr (CNode a) CEdge

------------------------------------------------------------
-- Creating simple Nodes and Eges
------------------------------------------------------------

-- | Node is actually just an Id (i.e. an Int)
--n0 :: (Show pl) => Node -> (Payload a) -> LNode (CNode pl)
n0 :: Int -> Payload pl -> LNode (CNode pl)
n0 i pl = (i, N0 pl)


{-
nnEmpty :: Node -> Label -> LNode (CNode pl)
nnEmpty i lbl = (i, NN lbl [])
-}


e0 :: Node -> Node -> LEdge CEdge
e0 i j = (i,j, E0 lbl)
        where
            lbl = show i ++ "->" ++ show j


getNodePayload :: LNode (CNode pl) -> pl
getNodePayload (id, N0 pl) = payload pl


------------------------------------------------------------
-- Unique Node IDs
------------------------------------------------------------
type NodePool pl = (Node, [LNode (CNode pl)])

createNode :: (Payload pl) -> State (NodePool pl) ()
createNode payload = do
    (id, ns) <- get
    put (id+1, (n0 id payload):ns)
    return ()

createNodes :: [Payload pl] -> State (NodePool pl) ()
createNodes = mapM_ createNode 

evalState0  gr = evalState gr 0
------------------------------------------------------------
-- Connecting Edges
------------------------------------------------------------


connect :: [(LNode (CNode pl),LNode (CNode pl))] -> [LEdge CEdge]
connect = map newEdge 

newEdge ((i,_), (j,_)) = e0 i j


type NodeEq pl = LNode (CNode pl) -> LNode (CNode pl) -> Bool

-- | Connect all nodes in nodes which satisfy an equality criterion
connectAll :: [LNode (CNode pl)] -> NodeEq pl-> [LEdge CEdge]
connectAll nodes eqf = [newEdge (from, to)| from<-nodes, to<-nodes, eqf from to]

type Projection a b = (Payload a) -> (Payload b) -- xxx


------------------------------------------------------------
-- Example Graph
------------------------------------------------------------

data ExPayload = Expl {
            pType    :: Char, 
            accepts  :: [String], 
            produces :: [String], 
            orig     :: String,
            dest     :: String
} deriving (Eq, Ord)

instance Show ExPayload where
        show pl = trc "show" $ " " ++ [pType pl] ++ "\\n" ++ (orig pl) ++ "\\n" ++ (dest pl)


pl t a p o d = let pl = Expl{
                            pType    = t,
                            accepts  = a,
                            produces = p,
                            orig     = o,
                            dest     = d
                        }
               in
                   PL { label   = show pl,
                        payload = pl
                      }


splitIn :: Int -> [a] -> [[a]]
splitIn n xs 
             | n == 0       = []
             | otherwise    = take piece xs : splitIn (n-1) (drop piece xs)
             where
                 len   = length xs
                 piece =  ceiling $ (fromIntegral len :: Double)/ fromIntegral n

exGraph :: Int -> State (NodePool ExPayload) (CGraph ExPayload)
          

exGraph fan = do   
    let collOffices       = (2*fan)  `named` "CO"
        centers           = (2*fan)  `named` "CE"
        myInwGroups       = (2*fan)  `named` "MIG"
        theirInwGroups    = (3*fan)  `named` "TIG"
        outwRuns          = fan      `named` "OR"
        sequenceRuns      = (4*fan)  `named` "SQ"
        walks             = (16*fan) `named` "WK"
        delOffices        = (6*fan)  `named` "DO"

    createNodes [extTransport  ["From"++collOffice] collOffice "CollArrival" 
                 | collOffice <- collOffices
                ]
    createNodes [unpack ["From"++collOffice] ["From"++collOffice++"Unp"] "CollArrival" 
                 | collOffice <- collOffices]

    createNodes [intTransport ["From"++collOffice++"Unp"] "CollArrival" "OutwArea"
                 | collOffice <- collOffices
                ]
    createNodes [extTransport ["From"++center] center "IctArrival"
                 | center <- centers
                            ]
    createNodes [unpack  ["From"++center] myInwGroups "IctArrival"
                 | center <- centers
                ]
    createNodes [intTransport [inwGroup] "IctArrival" "InwArea"
                 | inwGroup <- myInwGroups
                ]
    createNodes [outwSort ["From"++collOffice++"Unp"] (myInwGroups++theirInwGroups) "OutwArea"
                 | x<-outwRuns, collOffice <- collOffices
                ]
    createNodes [intTransport  [inwGroup] "OutwArea" "InwArea"
                 | inwGroup <- myInwGroups
                ]
    createNodes [inwSort [inwardGroup]  seqGroup "InwArea" 
                 |(inwardGroup, seqGroup) <- distribute myInwGroups sequenceRuns
                ]
    createNodes [intTransport  [seqGroup] "InwArea" "SeqArea"
                 |seqGroup <- sequenceRuns
                ]
    createNodes [seqSort  [sequenceRun] walk "SeqArea"
                 | (sequenceRun, walk) <- distribute sequenceRuns walks
                ]
    createNodes [intTransport [walk] "SeqArea" "DoDeparture"
                 | walk <- walks
                ]
    createNodes [extTransport walk  "DoDeparture" delOffice
                 | (delOffice, walk) <- distribute delOffices walks
                ]
    createNodes [intTransport inwardGroup "OutwArea" "IctDepart"
                 | (center, inwardGroup) <- distribute centers theirInwGroups
                ]
    createNodes [extTransport  inwardGroup "IctDepart" center
                 | (center, inwardGroup) <- distribute centers theirInwGroups
                ]

    (i,nodes) <- get
    let
        edges = connectAll nodes connectible


    return $ mkGraph nodes edges
        where
            named :: Int -> String -> [String]
            named n s = [s ++ show x | x <- [1..n]]
              
            -- associate groups of ys with xs
            distribute xs ys = zip xs (splitIn (length xs) ys)

            -- some shorthand commands
            extTransport products orig dest        = pl 'T' products products orig dest
            intTransport products orig dest        = pl 't' products products orig dest
            unpack       accepts produces location = pl 'U' accepts produces location location
            outwSort     accepts produces location = pl 'O' accepts produces location location
            inwSort      accepts produces location = pl 'I' accepts produces location location
            seqSort      accepts produces location = pl 'S' accepts produces location location

            -- can two nodes be connected
            intersects s1 s2 = intersect s1 s2 /= []
            connectible x y = let plx = getNodePayload x
                                  ply = getNodePayload y
                              in
                                  dest plx == orig ply && produces plx `intersects` accepts ply


------------------------------------------------------------
-- aggregating and dissecting individual nodes and edges
------------------------------------------------------------



------------------------------------------------------------
-- Aggregator functions
------------------------------------------------------------

type Aggregator pl = LNode (CNode pl) -> Label

byFrom :: Aggregator ExPayload
byFrom (n, N0 (PL label pl)) = orig pl

byFromTo :: Aggregator ExPayload
byFromTo (n, N0 (PL lbl pl)) = orig pl ++ "->" ++ dest pl


byFromToTyped :: Aggregator ExPayload
byFromToTyped (n, N0 (PL lbl pl)) = lbl'
  where
    lbl' = [pType pl] ++ ":" ++ (if orig pl == dest pl
                                 then orig pl
                                 else orig pl ++ "->" ++ dest pl
                                )

aggregate :: (Eq pl, Ord pl) => CGraph pl -> Aggregator pl -> [(Label, [Node])]
aggregate gr aggf = map relabel $ groups
  where
    groups     = groupWith aggf (labNodes gr)
    relabel ns = (aggf (head ns), map fst ns)


------------------------------------------------------------
-- Collapsing and expanding graphs
------------------------------------------------------------


-- Helpers

uniq :: (Eq a, Ord a) => [a]->[a]
uniq = nub . sort
a `without` b = O.minus (sort a)  (sort b)

setOrig, setDest :: Node -> Edge -> Edge
setOrig n (i,j) = (n,j)
setDest n (i,j) = (i,n)

type GraphTransform pl = CGraph pl -> CGraph pl


groupNodes :: State Int (CGraph pl) -> (Label,[Node]) ->  State Int (CGraph pl)
groupNodes graph (lbl,ids)  =  do
    gr     <- graph
    (id,_) <- get
    let addNewNode  = createNode (PL {label=lbl})
        delOldNodes = delNodes ids     :: GraphTransform pl

        -- Now the edges:
        oldEdgesTo     = [(toOld, old)   | old <- ids, toOld <- pre gr old]
        oldEdgesFrom   = [(old, fromOld) | old <- ids, fromOld <- suc gr old] 
        oldEdges       = oldEdgesTo ++ oldEdgesFrom
        oldEdgesWithin = [ (i,j) | (i,j) <- oldEdges, i `elem` ids, j `elem` ids]


        newEdges       = uniq $ map (setDest id+1)  (oldEdgesTo   \\ oldEdgesWithin) ++ 
                                map (setOrig id+1)  (oldEdgesFrom \\ oldEdgesWithin)
        delOldEdges    = delEdges oldEdges  :: GraphTransform pl
        -- beware: inserting edge between nodes wich do not exists causes "Irrefutable pattern failed"
        addNewEdges    = insEdges $ map (uncurry e0) newEdges                :: GraphTransform pl
        
    return $ (addNewEdges . delOldEdges . addNewNode . delOldNodes) gr


{-
exGraph2 :: State Int (CGraph ExPayload)
exGraph2 = groupNodes (exGraph 1) "foo"  [0,1,2,3,4,12,13,14,15] 


exGraph3 :: Int -> State Int (CGraph ExPayload)
exGraph3 fan = foldr groupNodes' gr nodeGroups
        where
            gr = exGraph fan
            nodeGroups :: [(Label,[Node])]
            nodeGroups = aggregate (evalState0 gr) byFromToTyped
            groupNodes' (lbl, ns) g= groupNodes g lbl ns


-}    


------------------------------------------------------------
-- Drawing
------------------------------------------------------------
draw graph = do
    let g = evalState0 graph
--        dot = "D:/Software/Graphviz/bin/dot"
        dot = "dot"
        format = "eps"
        options = " -Gnodesep=0.1  -Nstyle=filled,rounded -Nshape=box -Nfontname=Arial -Efontname=Arial -Epenwidth=3 "
    writeFile "xxx.dot" (graphviz' g)
    createProcess $ shell $ dot ++ " -T " ++ format ++  options  ++ "xxx.dot  > xxx." ++ format


