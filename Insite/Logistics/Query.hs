
module Query where
import Data.List
import Data.Ord
import GHC.Exts
import Control.Monad
type Join a b = [a] -> [b] -> [(a,b)]



-- | Use when ys is smaller then xs
-- ljoin :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
ljoin :: (a->b->Bool) -> Join a b
ljoin p xs ys = [(x,y) |x<-xs, y <- ys, p x y]


-- | use when xs is smaller then ys
rjoin :: (a->b->Bool) -> Join a b
rjoin p xs ys = [(x,y) |y<-ys, x <- xs, p x y]



exxs = [1000..1999] :: [Int]
exys = [2000..2999] :: [Int]
exzs = [3000..3999] :: [Int]

p1 p x _ = p x
p2 p _ y = p y

rflat (a, (b,c)) = (a,b,c)
lflat ((a,b),c) = (a,b,c)

(-->) = flip ($)

xxx = let 
          j2 = ljoin $ \z (x,y) -> odd z
      in     
          exxs
          --> (ljoin $ p1 even) exys
          --> j2 exzs 
          --> take 10 
          --> map rflat

rownum = [1..]
xxy = zip rownum [(x,y,z) | 
                  (r,x,y) <- zip3 rownum exxs (sortWith Down exys),
                  r < 1000,
                  y<-exys,
                  y == 2*x,
                  odd x,
                  z <- exzs,
                  z == 3*x
                 ]

check :: b -> Bool -> [b]
check b p = if p then return b else []

xxz = do
    x <- exxs 
    guard $ odd x
    (r1,x1) <- [(r,x) | r<- [1..10]]
    guard (r1<10)
    return (r1, x1)
