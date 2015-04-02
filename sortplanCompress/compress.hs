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
        return x = Parser $ \inp -> [(x,inp)]
        p >>= f  = Parser $ \inp -> concat [parse (f a) cs'| (a,cs') <- parse p inp]


instance Applicative Parser where
        pure = return
        (<*>) = ap

instance Functor Parser where
        fmap f pa = Parser $ \xs -> map g (parse pa xs)
                where
                    g (x,ys) = (f x, ys)

instance MonadPlus Parser where
  p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)
  mzero       = pFail
  
addable :: HashedSortplan -> Product -> (Sortcode->Bool)
addable hsp p = \c ->  case M.lookupLT c hsp of
                         Nothing     ->  True
                         Just (_,p') ->  p==p'


pFail = Parser (const [])

pSat :: (Sortcode->Bool) -> Parser Sortcode
pSat p = do
    c <- pSortcode
    if p c then return c else pFail

pSortcode :: Parser Sortcode
pSortcode = Parser f
  where
    f [] = []
    f (x:xs) =[(x,xs)]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                     [] -> []
                     (x:xs) -> [x])

pUpperBound :: (Sortcode->Bool)->Parser Sortcode
pUpperBound p = do
  lo <- pSortcode
  hi <- pSat p
  if hi == [] then return lo else return hi


pRange :: (Sortcode->Bool) -> Parser Range
pRange p = do
  lo <- pSortcode
  hi <- pUpperBound p
  return (lo,hi)



exSpl  = mkSortplan 1
exHash = asHashedSortplan exSpl
exProd = ((snd.head) exSpl)

xxx = parse (pRange (addable exHash 1))  exProd
            


run :: Show x => x -> IO()
run x = (timeIt . putStrLn . show) x
