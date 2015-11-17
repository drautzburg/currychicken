{-# LANGUAGE BangPatterns #-}
{-|
Module      : Logger
Description : - Combinators for creating 'Logger's
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

A 'Logger' can be constructed from list of Condition ('Cnd') and
Writer ('Wtr') pairs (See 'desLogger'). Whenever 'Cnd' holds, the
'Wtr' will be called to produce one or more log entires, which will be
/prepended/ to the log. The 'Cnd' holds a state such that the 'Cnd'
can change over time, allowing to express things like "every 1000".

The __performance__ of this module is determined by the time it takes
to check 'Cnd's and the time to produce logs.

The __size__ of this module is 39 lines of code (without comments, blank lines and example code)


-}

module Logger where
import Data.List
import System.TimeIt
import Text.Show.Pretty
import Data.Monoid
import Des

-- * Conditions 

-- | a 'Cnd' runs a check on __a__ and returns a Bool and a new version of
-- itself. The latter is required, e.g. for logging at fixed intervals
data  Cnd a = Cnd {checkCnd ::  a -> (Bool, Cnd a)}


-- | Lift a simple predicate to a stateless 'Cnd'
cndIf :: (a->Bool) -> Cnd a
cndIf p = Cnd $  \x -> (p x, cndIf p)

-- | Combine to 'Cnd's with a binary boolean operation.
cndOp :: (Bool->Bool->Bool) -> Cnd a -> Cnd a -> Cnd a
cndOp  op c1 c2  = Cnd $ \a -> let !(b1, c1') = checkCnd c1 a
                                   !(b2, c2') = checkCnd c2 a
                               in (b1 `op` b2, cndOp op c1' c2')

-- | Combine to 'Cnd's with "or"
cndOr :: Cnd a -> Cnd a -> Cnd a
cndOr = cndOp (||)


-- | Combine to 'Cnd's with "and"
cndAnd :: Cnd a -> Cnd a -> Cnd a
cndAnd = cndOp (&&)


-- | Create a stateful 'Cnd' such that each time the predicate holds,
-- a new Cnd is created.
cndStep :: (a->Bool) -> (a->Cnd a) -> Cnd a
cndStep p s = Cnd $ \a -> if p a then (True, s a)
                            else (False, cndStep p s)


-- * Writers

-- | A 'Wtr' returns some log-entires to be /prepended/ to the log.
type  Wtr a log = a -> log

-- | A /conditioned writer/, i.e. a 'Cnd' with associated 'Wtr'
type  CndWtr a log = (Cnd a, Wtr a log)

-- | run all 'CndWtr's in the provided lists and /prepend/ the new log entries to the log.
runCndWtrs ::  Monoid log => a -> log -> [CndWtr a log] -> (log, [CndWtr a log])
runCndWtrs a l cws = foldl' f (l,[]) cws
        where
            f  (l',cws') (c,w) = let (b,c') = checkCnd c a
                                     cws'' = (c',w):cws'
                                 in if b 
                                    then (w a <> l', cws'')
                                    else (l',        cws'')

-- * Logger for "Des"

-- | Construct a 'Logger' for "Des", i.e. on ('Timed' evt, dom) from 'CndWtr's
desLogger :: Monoid log => [CndWtr (Timed evt, dom) log] -> Logger evt dom log
desLogger cwss  = Lgr $ \ (tev,dom) l -> let (l', cwss') = runCndWtrs (tev,dom) l cwss
                                         in (l', desLogger cwss')

-- * Example code

-- | Example main
ex_main :: IO()
ex_main = timeIt $ putStrLn $ ppShow $ fst $ ex_onList ex_loggers [] [1..5000000]
        where
            ex_CndAt :: Int -> Cnd Int
            ex_CndAt x = Cnd $ \a -> (a==x, ex_CndAt x)

            ex_CndEvery x dx = let p a = a >= x
                                   s a = ex_CndEvery (a+dx) dx
                               in cndStep p s

            ex_wtr1 :: Wtr Int [String]
            ex_wtr1 a = ["Writer ex_wtr1 saw " ++ (show a)]

            ex_wtr2 :: Wtr Int [String]
            ex_wtr2 a = ["Writer ex_wtr2 saw " ++ (show a)]

            ex_loggers :: [CndWtr Int [String]]
            ex_loggers = 
                    [
                     (ex_CndAt 3 `cndOr` ex_CndAt 10000,                         ex_wtr1),
                     (ex_CndEvery 100000 100000,                                 ex_wtr2),
                     (cndIf (\x -> mod x 123456 == 0) `cndAnd` ex_CndAt 1728384, ex_wtr1)
                    ]


            ex_onList cws l0 xs = foldl f (l0,cws) xs
                    where 
                        f :: ([String], [CndWtr Int [String]]) -> Int -> ([String], [CndWtr Int [String]])
                        f (l,cws') a = runCndWtrs a l cws' 


