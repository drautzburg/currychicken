-- expriments with cartesian sets to construct Separations
import Data.Ranged.Ranges
import Data.Ranged.RangedSet
import Data.Ranged.Boundaries


srange :: (String, String) -> Range String
srange (l,h) = Range (BoundaryBelow l) (BoundaryAbove h)

irange :: (Int, Int) -> Range Int
irange (l,h) = Range (BoundaryBelow l) (BoundaryAbove h)


xxxr,xxyr :: RSet String

xxxr = makeRangedSet $ map srange [(show x++"500",show (x+1)++"000") | x <- [100000 .. 900000 ]]
xxyr = makeRangedSet $ map srange [(show x++"000",show x++"500") | x <- [100000 .. 900000 ]]

xxu = rSetUnion xxxr xxyr

type Separation = [(RSet String, RSet Int)]



exSep1 :: Separation
exSep1 = [(makeRangedSet [srange ("100","200"), srange ("300","350")], makeRangedSet [irange (1, 2)])]
exSep2 = [(makeRangedSet [srange ("150","250")], makeRangedSet [irange (1,2)])]

xxx = (makeRangedSet [srange ("150","250")], makeRangedSet [irange (1,1)])
xxy = (makeRangedSet [srange ("150","250")], makeRangedSet [irange (1,1)])

-- union of two rectangles
rUnion :: (RSet String, RSet Int) -> (RSet String, RSet Int) -> [(RSet String, RSet Int)]
rUnion (x1,y1) (x2,y2) = filter (\(a,b) -> not (rSetIsEmpty a) && not (rSetIsEmpty b))
                         [
--                           (rSetDifference x1 x2, y1) ,
                           (x1, rSetDifference y1 y2) ,
                           (rSetIntersection x1 x2, rSetIntersection y1 y2) ,
--                            (rSetDifference x2 x1,y2)
                           (x1, rSetDifference y2 y1)
                         ]


sUnion :: Separation -> Separation -> Separation
sUnion s1 [] = s1
sUnion [] s2 = s2
sUnion s1 (r:rs) = concatMap (rUnion r) s1 ++ sUnion s1 rs
