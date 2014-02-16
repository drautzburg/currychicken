%include lhs2TeX.fmt
\subsubsection{Tests}

\begin{code}
import Mail
import Test.QuickCheck
import Debug.Trace

instance Arbitrary Mpc
         where
           arbitrary = do
             mc <- arbitrary
             sc <- arbitrary
             day <- arbitrary
             return (Mpc mc sc day)

exampleMail = dailyMail 1 1 (1/6) 10000 
myTrace s val = trace (s ++ "=" ++ show val) val
\end{code}

\begin{code}
-- Popping a Mpc from emptyMail returns Nothing
prop_empty1 = (fst $ popMpc emptyMail) == Nothing

prop_pushPop :: Mpc -> Bool
prop_pushPop m_in@(Mpc c s d) = mpc == m_in && countMpcs m2 == 0
        where
            m1 = pushMpc m_in emptyMail
            (Just mpc,m2) = popMpc m1
\end{code}

\textbf{Access and Summing}

\begin{code}

-- Compare summing counts in the hash to counting Mailpieces
prop_sum :: [Mpc] -> Bool
prop_sum mpcs = (countMpcs mail) == (length mpcs)
        where
            mail = asMail mpcs


-- The largest Sortcode in mail must be less or equal to the largest possible Sortcode
prop_max :: Positive Double -> Positive Double -> Bool
prop_max (Positive alpha) (Positive lambda) = maxSc mail <= scMax lambda mvpd
        where
            mvpd = 100
            mail = dailyMail 2 alpha lambda mvpd 0


-- The sum of two halves must be equal to the whole
prop_sumWithin :: Positive Double -> Positive Double -> Bool
prop_sumWithin (Positive alpha) (Positive lambda) = loCount + hiCount == countMpcs mail
        where
            mvpd    = 10000
            mail    = dailyMail 2 alpha lambda mvpd 0
            upper   = 1 + scMax lambda mvpd
            mid     = upper `div` 2
            loCount = countMpcsWithin mail 0 mid
            hiCount = countMpcsWithin mail mid upper
\end{code}

\needspace{20pt}
\textbf{Distribution tests}

Distribution tests are not easy, due to the randomness of the
generated test data. The tests here compare the number of Mailpieces
in the upper half of the Sortcode range, with those in the lower
half. For an even distribution ($\alpha=1$) those two numbers should
be approximately the same. For $\alpha=2$ we expect 1/4 to be in the
lower half and 3/4 in the upper (because $P(\frac 1 2) = \frac 1 4$).


\begin{code}
proportion :: Int -> Int -> Double
proportion a b = (fromIntegral a) / (fromIntegral b)

-- Helper to test whether a given $\alpha$ produces the expected skew
testSkew :: Double -> Double -> Double -> Positive Double -> Bool
testSkew alpha minSkew maxSkew (Positive lambda)   
        -- discard some values, we don't like
        | lambda > 100.0 = True
        | lambda < 0.001 = True
        | otherwise = mySkew < maxSkew && mySkew > minSkew
        where
            mySkew  = proportion hiCount loCount
            mvpd    = 2000
            mail    = dailyMail 2 alpha lambda mvpd 0
            upper   = 1 + scMax lambda mvpd
            mid     = upper `div` 2
            loCount = countMpcsWithin mail 0 mid
            hiCount = countMpcsWithin mail mid upper

-- Expect $0.9 < proportion < 1.1$ for even distribution
prop_dis_a1 :: Positive Double -> Bool
prop_dis_a1 = testSkew 1.0 0.9 1.1

-- Expect $2.8 < proportion < 3.2$ for $\alpha=2$
prop_dis_a2 :: Positive Double -> Bool
prop_dis_a2 = testSkew 2.0 2.8 3.2

runTests :: IO()
runTests = do
    putStrLn "prop_pushPop"
    quickCheck prop_pushPop

    putStrLn "prop_empty1"
    quickCheck prop_empty1

    putStrLn "prop_sum"
    quickCheck prop_sum

    putStrLn "prop_max"
    quickCheck prop_max

    putStrLn "prop_sumWithin"
    quickCheck prop_sumWithin

    putStrLn "prop_dis_a1"
    quickCheck prop_dis_a1

    putStrLn "prop_dis_a2"
    quickCheck prop_dis_a2

\end{code}
