import Data.List

type Time = Int

------------------------------------------------------------
-- Timed and Queue
------------------------------------------------------------
data Timed a = T Time a deriving Show
sortByTime :: [(Timed a)] -> [(Timed a)]
sortByTime = sortBy (\(T t1 _) (T t2 _) -> compare t1 t2)


data Queue a =  Q {add :: a -> (Queue a),
                  del :: ((Queue a), a),
                  elm :: [a]
                 }

instance Show (Queue a) where
  show x = "Queue of length " ++ (show . length) elements 
    where
      elements = elm x

simpleQueue :: [(Timed a)] -> Queue (Timed a)
simpleQueue ta =
  let taSorted = sortByTime ta
  in
   Q {
     add = \a -> simpleQueue $ sortByTime (a:ta),
     del = (simpleQueue (tail taSorted), (head taSorted)),
     elm = taSorted
  }

exQueue = simpleQueue [T 10 "aa", T 2 "vv"]


------------------------------------------------------------
-- Process
------------------------------------------------------------
type Item = String
type Position = Int
data EventData = Available Item Position
               | Consumed Item Position deriving Show
data ProcessState a = PS a

instance Show Process where show p = info p

data Process = P {
  step :: Timed EventData -> (Process, Maybe (Timed EventData)),
  info :: String
  }

type Capacity = Int
type BufferState = Timed (Capacity, [Item])

simpleProcess :: Position -> ProcessState BufferState -> Process
simpleProcess p (PS (c,items)) = P {
  step = \(T t (Available item pos)) -> case (length items < c, pos) of
     (True, p) -> let p' = simpleProcess p (PS (c,item:items))
                      evt = (T t (Consumed item p))
                  in (p', Just evt)
     _         -> (simpleProcess p (PS (c,item:items)), Nothing)
     ,
  info = show items
  }

exProc = simpleProcess 1 (PS (2,[]))
-- step exProc (T 10 (Available "lkj" 1))
