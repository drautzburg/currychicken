{-# LANGUAGE BangPatterns #-}
import Data.List
import System.TimeIt
import Text.Show.Pretty

type Log = [String]

data  Cnd a = Cnd {checkCnd ::  a -> (Bool, Cnd a)}
type  Wtr a = a -> String
type  CndWtr a = (Cnd a, Wtr a)

-- most of the time no Cnd changes state and all Cnd return False and
-- thus no log is written

runLogger ::  a -> Log -> [CndWtr a] -> (Log, [CndWtr a])
runLogger a l cws = foldl' f (l,[]) cws
        where
            f  (l',cws') (c,w) = let (b,c') = checkCnd c a
                                     cws'' = (c',w):cws'
                                 in if b 
                                    then (w a : l', cws'')
                                    else (l',       cws'')


at :: Int -> Cnd Int
at x = Cnd $ \a -> (a==x, at x)

every :: Int -> Int -> Cnd Int
every x dx = Cnd $ \a -> if a>=x then (True, every (a+dx) dx)
                         else (False, every x dx)

cIf :: (Int->Bool) -> Cnd Int
cIf p = Cnd $  \x -> (p x, cIf p)

cndOp :: (Bool->Bool->Bool) -> Cnd a -> Cnd a -> Cnd a
cndOp  op c1 c2  = Cnd $ \a -> let !(b1, c1') = checkCnd c1 a
                                   !(b2, c2') = checkCnd c2 a
                               in (b1 `op` b2, cndOp op c1' c2')


cndOr :: Cnd a -> Cnd a -> Cnd a
cndOr = cndOp (||)


cndAnd :: Cnd a -> Cnd a -> Cnd a
cndAnd = cndOp (&&)


ex_wtr1 :: Wtr Int
ex_wtr1 a = "Writer ex_wtr1 saw " ++ (show a)

ext_wtr2 :: Wtr Int
ext_wtr2 a = "Writer ext_wtr2 saw " ++ (show a)

ex_loggers :: [CndWtr Int]
ex_loggers = 
        [
         (at 3 `cndOr` at 10000, ex_wtr1),
         (every 100000 100000, ext_wtr2),
         (cIf (\x -> mod x 123456 == 0) `cndAnd` at 1728384, ex_wtr1)
        ]


onList cws l0 xs = foldl' f (l0,cws) xs
        where 
            f :: (Log, [CndWtr Int]) -> Int -> (Log, [CndWtr Int])
            f (l,cws) a = runLogger a l cws 



main = timeIt $ putStrLn $ ppShow $ fst $ onList ex_loggers [] [1..5000000]


