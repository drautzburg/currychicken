import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
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
type Pair     = (Sortcode, Sortcode)
type Range    = (Sortcode, Sortcode)

mkSortplan :: Int -> Sortplan
mkSortplan rows = take 10 $ mkProduct 1 codes
  where
    mkProduct p cs = (p, take 10 cs) : mkProduct (p+1) (drop 10 cs)
    codes  = [1,1+rows ..]

type HashedSortplan = M.Map Sortcode Product
asHashedSortplan :: Sortplan -> HashedSortplan
asHashedSortplan spl = M.fromList codeProduct
  where
    codeProduct    = concatMap ungroup spl
    ungroup (p,cs) = [(c,p)| c<-cs]

type RangedSortplan = [(Product, [Range])]

rangeCompress :: Sortplan -> RangedSortplan
rangeCompress spl = map compressProduct spl
  where
    hspl = asHashedSortplan spl
    compressProduct :: (Product, [Sortcode]) -> (Product, [(Sortcode, Sortcode)])
    compressProduct (p,[]) = (p,[])
    compressProduct (p,cs) = (p, compressCodes p (zip cs (tail cs)))

    compressCodes :: Product -> [(Sortcode,Sortcode)] -> [(Sortcode, Sortcode)]
    compressCodes p cps = undefined
    
    asRange :: [(Sortcode,Sortcode)] -> (Sortcode,Sortcode)
    asRange (x:xs) = (fst x, snd $ last xs)
    
    addable :: Product -> Sortcode -> Bool
    addable p c = let match = M.lookupLT c hspl 
                  in False -- match == Nothing || (snd . fromJust) match == p


type ParseIn = [Sortcode]
newtype Parser a = Parser {parse :: ParseIn -> [(a, ParseIn)]}

instance Monad Parser where
        return x = Parser (\inp -> [(x,inp)])
        p >>= f  = Parser $ \ps -> case (parse p) ps of
                                       []        -> []
                                       [(v,out)] -> parse (f v) out

instance Applicative Parser where
        pure = return
        (<*>) = ap

instance Functor Parser where
        fmap f pa = Parser $ \xs -> map g (parse pa xs)
                where
                    g (x,ys) = (f x, ys)


addable :: HashedSortplan -> Product -> Parser Sortcode
addable hsp p = Parser prs
        where
            prs :: ParseIn -> [(Sortcode, ParseIn)]
            prs [] = []
            prs (c:cs) = case M.lookupLT c hsp of
              Nothing     ->  [(c, cs)]
              Just (_,p') ->  if p==p'
                              then [(c, cs)]
                              else []

pUpper :: HashedSortplan -> Product -> Parser Range
pUpper hsp p = do
      c <- addable hsp p
      y <- pRange hsp p
      return y

pSortcode :: Parser Sortcode
pSortcode = Parser f
  where
    f [] = []
    f (x:xs) =[(x,xs)]

  
pRange :: HashedSortplan -> Product -> Parser Range
pRange hsp p = do
    (lo,hi) <- pUpper hsp p
    return (lo, hi)



exSpl  = mkSortplan 1
exHash = asHashedSortplan exSpl

xxx = parse (pRange exHash 1) ((snd.head) exSpl)
            


run :: Show x => x -> IO()
run x = (timeIt . putStrLn . show) x
