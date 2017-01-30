-- expriments with cartesian sets to construct Separations
import Data.Ranged.Ranges
import Data.Ranged.RangedSet
import Data.Ranged.Boundaries


range :: Ord a => (a, a) -> Range a
range (l,h) = Range (BoundaryBelow l) (BoundaryAbove h)

xxxr,xxyr :: RSet String

xxxr = makeRangedSet $ map range [(show x++"500",show (x+1)++"000") | x <- [100000 .. 900000 ]]
xxyr = makeRangedSet $ map range [(show x++"000",show x++"500") | x <- [100000 .. 900000 ]]

xxu = rSetUnion xxxr xxyr

type Separation = [(RSet String, RSet Int)]



exSep1 :: Separation
exSep1 = [(makeRangedSet [range ("100","200"), range ("300","350")], makeRangedSet [range (1, 2)])]
exSep2 = [(makeRangedSet [range ("150","250")], makeRangedSet [range (1,2)])]

xxx = (makeRangedSet [range ("150","250")], makeRangedSet [range (1,1)])
xxy = (makeRangedSet [range ("150","250")], makeRangedSet [range (1,1)])

-- union of two rectangles
rUnion :: (RSet String, RSet Int) -> (RSet String, RSet Int) -> [(RSet String, RSet Int)]
rUnion (x1,y1) (x2,y2) = -- filter (\(a,b) -> not (rSetIsEmpty a) && not (rSetIsEmpty b))
                         [
                           (rSetDifference x1 x2, y1) ,
                           (rSetIntersection x1 x2, rSetIntersection y1 y2) ,
                           (rSetDifference x2 x1,y2)
                         ]


-- sUnion :: Separation -> Separation -> Separation
-- sUnion s1 s2 = concatMap (\(a,b) -> rUnion a b) (zip s1 s2)
