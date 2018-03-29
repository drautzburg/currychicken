{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plot where
import Text.Pretty.Simple
import Debug.Trace
import Data.Functor.Bind

-- * List Plots

-- | A Plot implemented as a list. It is not an instance of anything interesting
type Changes t a = [(t,a)]

-- | Take all elements of the Plot which occur before tx
before :: Ord t => t -> Changes t a -> Changes t a 
before _ []  = []
before tx ((t,c):cs) 
    | t < tx    = (t,c) : before tx cs
    | otherwise = []


-- | Shift time, but leave data untouched
delay :: Num t => t -> Changes t a -> Changes t a 
delay tx = map (\(t,c) -> (t+tx,c))

-- | Get the start time of the first element. Fails when Plot is empty
start :: Changes t a -> t
start = fst . head 

cMap :: (a->b) -> Changes t a -> Changes t b
cMap f = let f' (t,a) = (t, f a)
         in map f' 
    
-- | Explicit monadic join
cJoin :: forall t a. (Ord t, Num t) =>  Changes t (Changes t a) -> Changes t a
cJoin css = let delayed = map (\(t0,p0) -> delay t0 p0) css :: [Changes t a]
                cut :: [Changes t a] -> Changes t a
                cut []     = []
                cut (c:[]) = c 
                cut (c:cs) = case head cs of
                                [] -> cut (c : tail cs)
                                c' -> before (start c') c  ++ cut cs
            in cut delayed


list :: (Show a) => a -> IO ()
list a = pPrint a

foo :: a -> a
foo a = trace "foo" a

exa, exb :: Changes Int String
exa = [(dt, "a-" ++ show dt) | dt <- [10,12..]]
exb = [(dt, "b-" ++ show dt) | dt <- [3..]]

exc :: Changes Int (Changes Int String)
exc = do
    (t,a) <- exa
    return (t, [(dt, "c-" ++ a ++ "-" ++ show dt) | dt <- [5..]])
    

-- * Wrapped Plots

data  Plot t a = Plot {
    changes :: Changes t a
    }
    deriving (Eq, Show)

instance Functor (Plot t) where
    fmap f = Plot . cMap f . changes


instance (Num t, Ord t) => Apply (Plot t) where
    fs <.> as = fs >>- (<$> as)

            
instance (Num t, Ord t) => Bind (Plot t) where
    join = Plot . cJoin . changes . fmap changes 


pa = Plot [(0,10),(20,20)] :: Plot Double String
pf = Plot [(0,(+1)),(25,(+2))] :: Plot Double (Int->Int)

data Renderer :: Plot t a -> Plot t a
