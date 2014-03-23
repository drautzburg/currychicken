import Data.List
import Data.List.Ordered
import Debug.Trace
import Text.Printf
import Text.Show.Pretty as Pr

type Day = Int

class Behavior b where
        at :: (Eq a) => b a -> Day -> a

between :: (Eq v, Behavior b) => Day -> Day -> b v -> [(Day, v)]
between d1 d2 bhvr = zip [d1..d2] (map (at bhvr) [d1 .. d2])

weekFollowing :: (Eq v, Behavior b) => Day -> b v ->  [(Day, v)]
weekFollowing day bhvr = between day (day+6) bhvr


------------------------------------------------------------
type (Change v) = (v, [(Day,v)])
------------------------------------------------------------

insertChange :: Day -> v -> (Change v) -> (Change v)
insertChange  d x (def, chgs) = (def, chgs')
        where
            clean = filter (\(dx,_) -> dx /= d) chgs 
            chgs' = insertBagBy dayOrder (d,x) clean
            dayOrder (d1,_) (d2,_) = compare d1 d2

valueAt :: (Change v) -> Day -> v
valueAt (def,[]) _ = def
valueAt (def, ((d,v):cs)) day 
           | d > day   = def
           | otherwise = valueAt (v, cs) day

normally a = (a, []) 


------------------------------------------------------------
data Displace v = Displace (Change v) deriving (Eq, Show)
------------------------------------------------------------
instance Behavior Displace where
        at (Displace chg) day = valueAt chg day

exampleDisplace = Displace $ 
    (insertChange 2 "two")   $
    (insertChange 8 "eight") $ 
    (insertChange 4 "four")  $
    (insertChange 10 "ten") (normally "")               


------------------------------------------------------------
data Repeat v = Repeat (Change v)
                        deriving (Eq)
------------------------------------------------------------

instance Behavior Repeat where
        at (Repeat chg) day = valueAt chg (day `mod` 7)


exampleRepeat = Repeat $
     (insertChange 0 "Even")  $
     (insertChange 5 "End") $ 
     (insertChange 2 "Even")  $ 
     (insertChange 1 "Odd")  (normally "") 


asPatterns :: (Ord v, Show v) => [(Day, v)] -> [(v, String)]
asPatterns betweens = map combine $ group $ betweens
        where
            group xs  = let val cmp l1 l2 = cmp (snd l1) (snd l2)
                        in groupBy (val (==)) $ 
                           sortBy (val compare) xs
            combine xs = let value = snd $ head xs
                             pattern = foldr f "-------" xs
                             f (d,v) pat 
                                     = let d' = d `mod` 7
                                       in
                                           (take d' pat) 
                                           ++ ["MTWTFSS" !! d' ] 
                                           ++ (drop (d'+1) pat)
                         in (value, pattern)

instance (Ord v, Show v) => Show (Repeat v) where
       show x = "Repeat " ++ (show $ asPatterns $ between 0 6 x)


------------------------------------------------------------
data Evolution v = Evolution (Displace (Repeat v))
                 deriving (Eq,Show)
------------------------------------------------------------

instance Behavior Evolution where
        at (Evolution displc) day = at repeat day
                where
                    repeat = at displc day


exampleEvolution = 
        let defaultWeek = Repeat $
                          (insertChange 0 "DefaultWkday")$
                          (insertChange 5 "DefaultWkend")  
                          (normally "") 
            winterWeek = Repeat $
                          (insertChange 0 "WinterEven")  $
                          (insertChange 5 "WinterWkend") $ 
                          (insertChange 2 "WinterEven")  $ 
                          (insertChange 1 "WinterOdd")  
                          (normally "") 
            summerWeek = Repeat $
                          (insertChange 0 "SummerEven")  $
                          (insertChange 5 "DefaultWkend")$ 
                          (insertChange 2 "SummerEven")  
                          (normally "") 
        in Evolution $ Displace $
                   (insertChange 40 winterWeek) $
                   (insertChange 80 summerWeek)
                   (normally defaultWeek)


--exceptionDay :: Eq v => Day -> v -> Evolution v -> Evolution v
exceptionDay  day val (Evolution (Displace chg)) = 
        let 
            nextRep = valueAt chg (day+1)
            xcptRep = (Repeat (normally val))
        in  Evolution $ Displace $
              (insertChange day (xcptRep))  $
              (insertChange (day+1) nextRep) 
              chg



exc1 = exceptionDay 39 "Ex39"
exc2 = exceptionDay 40 "Ex40"