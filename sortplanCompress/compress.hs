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


type ParseIn = (HashedSortplan, [Sortcode])
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


addable :: Product -> Parser Sortcode
addable p = Parser prs
        where
            prs :: ParseIn -> [(Sortcode, ParseIn)]
            prs (rsp,[]) = []
            prs (rsp,(c:cs)) = case M.lookupLT c rsp of
                    Nothing     ->  [(c, (rsp,cs))]
                    Just (_,p') ->  if p==p'
                                    then [(c, (rsp,cs))]
                                    else []




pRange :: Product -> Parser Range -> Parser Range
pRange p prg = do
    (lo,hi) <- prg
    hi'     <- addable p
    return (lo, hi')
    
    



            


run :: Show x => x -> IO()
run x = (timeIt . putStrLn . show) x
