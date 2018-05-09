
-- print an array of strings


aofs = ["one","two","three"]

paofs1 :: [String] -> IO()
paofs1 [] = return ()
paofs1 (s:ss) = do
    putStrLn s
    paofs1 ss

-- this is the same as    
paofs2 :: [String] -> IO()
paofs2 [] = return ()
paofs2 (s:ss) = putStrLn s >> paofs2 ss

-- the left part has the type
-- λ> :t (>>) (putStrLn "xx") 
-- (>>) (putStrLn "xx") :: IO b -> IO b

-- It is a function which expects an IO b as argument. This argument
-- is the remainder of the IO action.


