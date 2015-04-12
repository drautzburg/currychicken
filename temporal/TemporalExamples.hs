{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Temporal
import Text.PrettyPrint.GenericPretty
--import System.Eval.Haskell
import Language.Haskell.Interpreter
import Data.Either
import System.IO
import qualified Data.Foldable as F
-- instance Generic (Change a)
--instance (Out a) => Out (Change a)
--instance (Out a) => Out (Temporal a)

cmd = "lkj"

run1 s = runInterpreter $ do
             loadModules ["Temporal"] 
             setImportsQ [("Prelude", Nothing), 
                          ("Temporal", Nothing), 
                          ("Text.PrettyPrint.GenericPretty", Nothing),
                          ("Data.Foldable", Just "F"),
                          ("Control.Applicative", Nothing)
                         ]  
             res <- eval ("take 900 $ pretty $ " ++ s)
             (return res) 
       :: IO(Either InterpreterError String)

trunc s = let chars = 300
              x = take chars s
              y = drop chars s
          in case y of 
                 [] -> x
                 _ -> x ++ "..."

run tag s = do
    h <- openFile (tag++".inc") WriteMode
    hPutStrLn h $ "{\\small\\begin{verbatim}"
    hPutStrLn h $ "*Main> " ++ s
    x <- run1 s
    let out x = hPutStrLn h x >> putStrLn x
    case x of
        Right y -> out (trunc $ read y)
        Left  e -> out(show e)
    hPutStrLn h $ "\\end{verbatim}}"
    hClose h

main = do
    run "exNat" "exNat"
    run "natFoldr" "F.foldr (+) 0 exNat"
    run "natFmap" "fmap (^2) exNat"
    run "natSum" "F.sum exNat"
    run "appl1" "(*) <$> exNat <*> ext2"
    run "appl2" "tNubBy (==) $ (*) <$> exNat <*> ext2"
    run "cyc1" "tCycle 12 ext2"
    run "cyc2" "tCycle 4 ext2"
    run "cyc3" "tCycle 3  exNat"
