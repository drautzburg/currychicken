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
import Control.Arrow
import System.Info
trc s x = trace (s ++ ":" ++show x) x

type Label = String


data CNode pl = N0 pl | NN Label   deriving (Eq)
instance (Show pl) => Show (CNode pl)
        where 
            show (N0 pl) = show pl
            show (NN lbl) = lbl


data CEdge = E0 Label deriving (Eq)
instance Show CEdge 
        where 
            show (E0 s) = "" -- s


type CGraph pl = P.Gr (CNode pl) CEdge
cempty = empty :: CGraph pl

------------------------------------------------------------
-- Creating simple Nodes and Eges
------------------------------------------------------------

-- | Node is actually just an Id (i.e. an Int)

n0 :: Int -> pl -> LNode (CNode pl)
n0 i pl = (i, N0 pl)


e0 :: Node -> Node -> LEdge CEdge
e0 i j = (i,j, E0 lbl)
        where
            lbl = show i ++ "->" ++ show j


getNodePayload :: LNode (CNode pl) -> pl
getNodePayload (id, N0 pl) = pl


------------------------------------------------------------
-- Graph manipulation
------------------------------------------------------------

type GraphTransform gr pl = gr (CNode pl) CEdge -> gr (CNode pl) CEdge


createNodes :: DynGraph gr => [CNode pl] -> GraphTransform gr pl
createNodes cns gr = insNodes nodes gr
        where
            nodes = zip (newNodes (length cns) gr) cns

createNode :: DynGraph gr => CNode pl -> GraphTransform gr pl
createNode cn = createNodes [cn]


type NodeEq pl = LNode (CNode pl) -> LNode (CNode pl) -> Bool

connectAll :: DynGraph gr => NodeEq pl-> GraphTransform gr pl
connectAll eqf gr = insEdges [edge from to| from<-nodes, to<-nodes, eqf from to] gr
        where
            nodes            = labNodes gr
            edge (i,_) (j,_) = e0 i j

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
        show pl = [pType pl] ++ "\\n" ++ if (orig pl == dest pl)
                                         then (orig pl)
                                         else (orig pl)++"\\n" ++ (dest pl)


expl  :: Char -> [String] -> [String] -> String -> String -> CNode ExPayload
expl t a p o d = N0 $ Expl{
                   pType    = t,
                   accepts  = a,
                   produces = p,
                   orig     = o,
                   dest     = d
               }

splitIn :: Int -> [a] -> [[a]]
splitIn n xs 
             | n == 0       = []
             | otherwise    = take piece xs : splitIn (n-1) (drop piece xs)
             where
                 len   = length xs
                 piece =  ceiling $ (fromIntegral len :: Double)/ fromIntegral n
  

exGraph :: DynGraph gr => Int -> GraphTransform gr ExPayload
exGraph fan = 
  let collOffices       = (2*fan)  `named` "CO"
      centers           = (2*fan)  `named` "CE"
      myInwGroups       = (2*fan)  `named` "MIG"
      theirInwGroups    = (3*fan)  `named` "TIG"
      outwRuns          = fan      `named` "OR"
      sequenceRuns      = (4*fan)  `named` "SQ"
      walks             = (9*fan)  `named` "WK"
      delOffices        = (3*fan)  `named` "DO"
  in
    createNodes [extTransport  ["From"++collOffice] collOffice "CollArrival" 
                 | collOffice <- collOffices]
    >>>
    createNodes [unpack ["From"++collOffice] ["From"++collOffice++"Unp"] "CollArrival" 
                 | collOffice <- collOffices]
    >>>
    createNodes [intTransport ["From"++collOffice++"Unp"] "CollArrival" "OutwArea"
                 | collOffice <- collOffices]
    >>>
    createNodes [extTransport ["From"++center] center "IctArrival"
                 | center <- centers]
    >>>
    createNodes [unpack  ["From"++center] myInwGroups "IctArrival"
                 | center <- centers]
    >>>
    createNodes [intTransport [inwGroup] "IctArrival" "InwArea"
                 | inwGroup <- myInwGroups]
    >>>
    createNodes [outwSort ["From"++collOffice++"Unp"] (myInwGroups++ (map (++ "Unp")theirInwGroups)) "OutwArea"
                 | x<-outwRuns, collOffice <- collOffices]
    >>>
    createNodes [intTransport  [inwGroup] "OutwArea" "InwArea"
                 | inwGroup <- myInwGroups]
    >>>
    createNodes [inwSort [inwardGroup]  seqGroup "InwArea" 
                 |(inwardGroup, seqGroup) <- distribute myInwGroups sequenceRuns]
    >>>
    createNodes [intTransport  [seqGroup] "InwArea" "SeqArea"
                 |seqGroup <- sequenceRuns]
    >>>
    createNodes [seqSort  [sequenceRun] walk "SeqArea"
                 | (sequenceRun, walk) <- distribute sequenceRuns walks]
    >>>
    createNodes [intTransport [walk] "SeqArea" "DoDeparture"
                 | walk <- walks]
    >>>
    createNodes [extTransport walk  "DoDeparture" delOffice
                 | (delOffice, walk) <- distribute delOffices walks]
    >>>
    createNodes [intTransport (map (++ "Unp") inwardGroup) "OutwArea" "IctDepart"
                 | (center, inwardGroup) <- distribute centers theirInwGroups]
    >>>
    createNodes [pack (map (++ "Unp") inwardGroup) ["To"++center]  "IctDepart"
                 | (center, inwardGroup) <- distribute centers theirInwGroups]
    >>>
    createNodes [extTransport  ["To"++center] "IctDepart" center
                 | center <- centers]
    >>>
    connectAll connectible 
        where

            named :: Int -> String -> [String]
            named n s = [s ++ show x | x <- [1..n]]
              
            -- associate groups of ys with xs
            distribute xs ys = zip xs (splitIn (length xs) ys)

            -- some shorthand commands
            extTransport products orig dest        = expl 'T' products products orig dest
            intTransport products orig dest        = expl 't' products products orig dest
            unpack       accepts produces location = expl 'U' accepts produces location location
            pack         accepts produces location = expl 'P' accepts produces location location
            outwSort     accepts produces location = expl 'O' accepts produces location location
            inwSort      accepts produces location = expl 'I' accepts produces location location
            seqSort      accepts produces location = expl 'S' accepts produces location location

            -- can two nodes be connected?
            intersects s1 s2 = intersect s1 s2 /= []
            connectible x y = let plx = getNodePayload x
                                  ply = getNodePayload y
                              in
                                  dest plx == orig ply && produces plx `intersects` accepts ply



------------------------------------------------------------
-- Aggregator functions
------------------------------------------------------------

type Aggregator pl = LNode (CNode pl) -> Label

byFrom :: Aggregator ExPayload
byFrom (n, N0 pl) = orig pl


byFromTo :: Aggregator ExPayload
byFromTo (n, N0 pl) = orig pl ++ "->" ++ dest pl


byFromToTyped :: Aggregator ExPayload
byFromToTyped (n, N0 pl) = lbl'
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

--type GraphTransform pl = CGraph pl -> CGraph pl

groupNodes :: DynGraph gr => (Label,[Node])  -> GraphTransform gr pl
groupNodes (lbl,ids) gr =
  let
    id = head $ newNodes 1 gr
    oldEdgesTo     = [(toOld, old)   | old <- ids, toOld <- pre gr old] 
    oldEdgesFrom   = [(old, fromOld) | old <- ids, fromOld <- suc gr old] 
    oldEdges       = oldEdgesTo ++ oldEdgesFrom
    oldEdgesWithin = [ (i,j) | (i,j) <- oldEdges, i `elem` ids, j `elem` ids]
    newEdges       = map (uncurry e0) $ uniq $ 
                                map (setDest id)  (oldEdgesTo   \\ oldEdgesWithin) ++ 
                                map (setOrig id)  (oldEdgesFrom \\ oldEdgesWithin)
  in
      (
      insNode (id, (NN lbl)) 
      >>> delNodes ids
      >>> delEdges oldEdges
      >>> insEdges newEdges
      ) gr

--    insEdges newEdges $ delEdges oldEdges $ (delNodes ids) $ insNode (id, (NN lbl)) gr

exGraph2 :: CGraph ExPayload
exGraph2 =
  let gr = (exGraph 1) cempty
  in groupNodes ("foo",  [0,1,2,3,4,12,13,14,15]) gr
    

exGraph3 :: Int -> CGraph ExPayload
exGraph3 fan =
  let gr = (exGraph fan) empty
  in foldr groupNodes gr (aggregate gr byFromToTyped)

------------------------------------------------------------
-- Drawing
------------------------------------------------------------
--draw :: (DynGraph gr, Show pl)  => GraphTransform gr pl -> IO ()
draw gr = do
    let 
        dot = case os of
                  "linux" -> "dot"
                  _ -> "D:/Software/Graphviz/bin/dot"
        format = "eps"
        options = " -Gnodesep=0.1  -Nstyle=filled,rounded -Nshape=box -Nfontname=Arial -Efontname=Arial -Epenwidth=3 "
    writeFile "xxx.dot" (graphviz' gr)
    createProcess $ shell $ dot ++ " -T " ++ format ++  options  ++ "xxx.dot  > xxx." ++ format
    return ()

