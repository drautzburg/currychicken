import SingleDelivery
import Mail
main = do
        putStrLn $ show $ map countMpcs (take 40 $ mail 1 1 0.1 20000 0)
        return ()
