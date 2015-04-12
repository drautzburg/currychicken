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
type Sortplan = [(Product, Sortcode)]
type Pair     = (Sortcode, Sortcode)
type Range    = (Sortcode, Sortcode)
type RangedSortplan = [(Product, [Range])]
type ProductSortplan = [(Product, [Sortcode])]
type HashedSortplan = M.Map Sortcode Product

computeOutlets :: Int -> Int -> [Int] -> Sortplan
computeOutlets nParts nIgs codes = map f (zip codes [0..])
        where
            f (c,ix) = let ig = ix `div` nIgs
                           -- o3 = ix `mod` nIgs
                           o2 = ix `div` nParts
                           o1 = ix `mod` nParts
                       in (c, nIgs*ig + o1)

-- asProductSortplan :: Sortplan -> ProductSortplan
asProductSortplan spl = foldr f M.empty spl
        where
            f (c,p) m = case M.lookup c m of
                            Nothing -> M.insert p [c] m
                            Just cs -> M.adjust (id) p m

asHashedSortplan :: Sortplan -> HashedSortplan
asHashedSortplan spl = M.fromList spl

{-
addable :: HashedSortplan -> Product -> (Sortcode->Bool)
addable hsp p = \c ->  case M.lookupLT c hsp of
                         Nothing     ->  True
                         Just (_,p') ->  p==p'


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


pMany1 :: Parser a -> Parser [a]
pMany1 p = do
    a <- p
    as <- pMany p
    return (a:as)

pMany :: Parser a -> Parser [a]
pMany p = pMany1 p +++ return []

pLast :: Parser a -> Parser a
pLast p = do
    a <- p
    as <- pMany p
    return $ last (a:as)
    
span'' :: (a -> Bool) -> [a] -> (Maybe a,[a])
span'' p xs = let (s,rest) = span p xs 
              in case s of
                     [] -> (Nothing, rest)
                     _ -> (Just $ last s, rest)

pUpperBound :: (Sortcode->Bool)->Parser Sortcode
pUpperBound' p = do
  hi <- pLast (pSat p)
  return hi

pUpperBound p = Parser $ \inp -> let (ub,rest) = span'' p inp
                                 in case ub of
                                        Nothing -> []
                                        Just u  -> [(u, rest)]

pRange :: (Sortcode->Bool) -> Parser Range
pRange p = do
  lo <- pSortcode
  hi <- pUpperBound p
  return (lo,hi)


rangify :: Sortplan -> RangedSortplan
rangify sp = map rangeProduct sp
        where
            hsp = asHashedSortplan sp
            rangeProduct :: (Product,[Sortcode]) -> (Product,[Range])
            rangeProduct (p,cs) = let add = addable hsp p
                                      rgs = parse (pRange add) cs
                                  in (p, map fst rgs)

exSpl  = mkSortplan 4000000 2
exHash = asHashedSortplan exSpl
exProd = ((snd.head) exSpl)

xxx = parse (pRange (addable exHash 1))  exProd
xxy = rangify exSpl
            


run :: Show x => x -> IO()
run x = (timeIt . putStrLn . show) x

main = do
    putStrLn $ show (rangify exSpl)
-}