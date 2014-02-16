import System.Random
import Data.List
import Data.Tuple
type Mpc = (Char, Int)
type Mpcs = [Mpc]

hash :: Int -> Int
hash x = decimals (fusc x)
        where
            fusc :: Int -> Float
            fusc a = sqrt (fromIntegral (a+1234))
            decimals :: Float -> Int
            decimals b = floor (10000 * (b - fromIntegral (floor b)))

randomMpcs :: Int->Int -> [(Char, Int)]
randomMpcs count max = map(\x -> (mailClass x, x)) randomInts 
        where 
            randomInts = take count . randomRs (0, max) . mkStdGen $ 1
            mailClass x
                    | (mod (hash x) 4 == 0) = 'B'
                    | otherwise = 'A'


type Judge = Mpc -> Mpcs -> Mpcs -> Mpcs -> Bool

isX :: Judge
isX (c,s) _ _ _
        | c=='A' = True
        | mod s 2 == 0 = True
        | otherwise = False

isY :: Judge
isY (c,s) _ _ _
        | c=='A' = True
        | mod s 2 == 1 = True
        | otherwise = False


separate :: Judge -> Mpcs -> Mpcs -> Mpcs -> Mpcs -> (Mpcs,Mpcs,Mpcs)
separate _ [] good bad  yds = ([], good, bad)
separate judge ((c,s):xs) good bad yds = separate judge xs g b yds
        where
            g | judge (c,s) good bad yds= (c,s):good
              | otherwise = good
            b | not $ judge (c,s) good bad  yds = (c,s):bad
              | otherwise = bad

separateXy :: Mpcs -> Mpcs -> Mpcs -> Mpcs -> (Mpcs,Mpcs,Mpcs)
separateXy mpcs good bad yds = separate isX mpcs good bad yds

countSingles :: Mpcs -> Int
countSingles mpcs = length $ filter singles $ groupBy sameSortcode mpcs
        where
            sameSortcode (c1,s1) (c2,s2) = (s1 == s2)
            singles xs = (length xs == 1)
                    

iterDays :: [Judge] -> [Mpcs] -> Int
iterDays js bs = iterDays' js bs [] 0
        where
            iterDays' :: [Judge] -> [Mpcs] -> Mpcs -> Int -> Int
            iterDays' js [] yds sgls = sgls

            iterDays' (j:js) (b:bs) yds sgls = iterDays' js bs  bad (sgls + countSingles good)
                    where
                        (mpcs,good,bad) = separate j b [] [] yds


\end{code}