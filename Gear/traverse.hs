import Data.Traversable
import Control.Applicative
import Text.Pretty.Simple

-- pretty-print something
pretty a = do
    putStrLn "\\begin{verbatim}"
    pPrintNoColor a
    putStrLn "\\end{verbatim}"

-- pretty-print a list, showing the first f and the last l elements
prettyL f l as = do
    putStrLn "\\begin{verbatim}"
    pPrintNoColor (take f as)
    putStrLn "[...]"
    pPrintNoColor (drop (length as - l) as)
    putStrLn "\\end{verbatim}"

type Time = Int

------------------------------------------------------------
data Event a = Evt {
------------------------------------------------------------
    evtt ::Time,
    evtv :: a
    }deriving (Eq, Show)

-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- t = Event Traversible
-- f = []    Applicative


instance Functor Event where
    fmap f (Evt t a) = Evt t (f a)

instance Foldable Event where
    foldMap f (Evt t a) = f a
    foldr f z (Evt t a) = f a z

instance Traversable Event where
    traverse f (Evt t a) = Evt t <$> f a

------------------------------------------------------------
data Events a = Evts {
------------------------------------------------------------
    evts :: [Event a]
    } deriving (Eq, Show)

instance Functor Events where
    fmap f = Evts . (fmap . fmap)  f . evts

-- fab x (f fab b0 xs)
instance Foldable Events where
    foldr f z (Evts []) = z
    foldr f z (Evts (e:es)) = foldr f z e xxx


--instance Traversable Events where
--    traverse = 
traverse1 :: Applicative f => (Event a -> f b) -> Events a -> f [b]
traverse1 f xs = traverse f (evts xs)

traverse2 :: Applicative f => (a -> f b) -> Events a -> f [b]
traverse2 f xs = traverse (f.evtv) (evts xs)


-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- traverse :: (Applicative f) => (a -> f b) -> [a] -> [(t b)]

-- traverse  :: Applicative f => (a -> f b) -> Events a -> f (Events b)

ex :: Int -> Event [String]
ex t = Evt t [show tx | tx <- [t..t+2]]

exs :: Events  [String]
exs = Evts $ map ex [1..3]

flatten :: Events [a] -> Events a
flatten xs = Evts $ concatMap (traverse id) (evts xs)
