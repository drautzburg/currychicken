import Data.List
import Data.Maybe
import Debug.Trace
import qualified  Data.Map.Lazy as M
import System.TimeIt

tr x = trace ("<" ++ show x ++ ">") x

transformCode cols rows c = cols * row + col
    where
      col = c `div` rows
      row = c `mod` rows



type Product  = Int
type Sortcode = Int
type Sortplan = [(Product, [Sortcode])]

mkSortplan :: Int -> Sortplan
mkSortplan rows = take 10 $ mkProduct 1 codes
  where
    mkProduct p cs = (p, take 10 cs) : mkProduct (p+1) (drop 10 cs)
    codes  = [1,1+rows ..]

type HashedSortplan = M.Map Sortcode Product
asHashedSortplan :: Sortplan -> HashedSortplan
asHashedSortplan spl = M.fromList codeProduct
  where
    codeProduct = concatMap (\(p,cs) -> [(c,p)| c<-cs]) spl

type RangedSortplan = [(Product, [(Sortcode,Sortcode)])]

rangeCompress :: Sortplan -> RangedSortplan
rangeCompress spl = map compressProduct spl
  where
    hspl = asHashedSortplan spl
    compressProduct :: (Product, [Sortcode]) -> (Product, [(Sortcode, Sortcode)])
    compressProduct (p,[]) = (p,[])
    compressProduct (p,cs) = (p, compressCodes p (zip cs (tail cs)))

    compressCodes :: Product -> [(Sortcode,Sortcode)] -> [(Sortcode, Sortcode)]
    compressCodes p cps = foldl add ([], undefined) cps
      where
        add (res,lo) (l,h)
          | addable p (l,h) = (res, lo)
          | otherwise       = (res ++ [(lo, l)], h)

    asRange :: [(Sortcode,Sortcode)] -> (Sortcode,Sortcode)
    asRange (x:xs) = (fst x, snd $ last xs)
    
    addable :: Product -> (Sortcode, Sortcode) -> Bool
    addable p (lo,hi) = let match = M.lookupLT hi hspl 
                        in False -- match == Nothing || (snd . fromJust) match == p


run :: Show x => x -> IO()
run x = (timeIt . putStrLn . show) x
