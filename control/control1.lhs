\begin{code}
import qualified Data.Set as S



  
data SkillSet = Skills (S.Set String) deriving Show
includesSkills (Skills a) (Skills b) = b `S.isSubsetOf` a


data Resource  = Ers String SkillSet | Qrs Int SkillSet | NoResource
               deriving (Show)
data Request   = Erq String SkillSet | Qrq Int SkillSet | NoRequest
               deriving (Show)

class Skilled a where
  skills     :: a -> SkillSet
  satisfies  :: (Skilled b) => a -> b -> Bool
  satisfies  a b =  (skills a)  `includesSkills` (skills b)

instance Skilled Resource where
  skills (Ers _ skills) = skills
  skills (Qrs _ skills) = skills

instance Skilled Request where
  skills (Erq _ skills) = skills
  skills (Qrq _ skills) = skills


class Countable a where
  count :: a -> Int
  minus :: a -> Int -> a
  cmin  :: (Countable b) => a -> b -> Int
  cmin x y = min (count x) (count y)

instance Countable Resource where
  count NoResource       = 0
  count (Ers _ _)        = 1
  count (Qrs c _)        = c
  
  minus res 0            = res
  minus rs@(Ers _ _) 1   = NoResource
  minus (Qrs c s) n
        | c > n          = Qrs (c-n) s
        | c == n         = NoResource

instance Countable Request where
  count NoRequest        = 0
  count (Erq _ _)        = 1
  count (Qrq c _)        = c
  
  minus req 0            = req
  minus rq@(Erq _ _) 1   = NoRequest
  minus (Qrq c s) n
        | c > n          = Qrq (c-n) s
        | c == n         = NoRequest
  
instance Countable Int where
  count x    = x
  minus x y  = x - y

satisfy NoResource rq = (NoResource, rq)
satisfy res NoRequest = (res, NoRequest)

satisfy rs@(Ers rsName rsSkills) rq@(Erq rqName  rqSkills)
  | rsName == rsName  = (NoResource, NoRequest)
  | otherwise         = (rs, rq)

satisfy resource request
  | resource `satisfies` request  = (resource `minus` x, request `minus` x)
  | otherwise                     = (resource, request)
  where
    x = cmin resource request
\end{code}
