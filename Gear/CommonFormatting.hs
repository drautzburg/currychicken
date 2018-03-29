module CommonFormatting (
    pretty,
    prettyL,
    verb,
    redir,
    clean,
    test
    )
where

import qualified Data.Char
import qualified Text.Pretty.Simple as P
import qualified Data.Text.Lazy as T
import qualified Data.List as L
import qualified Sound.MIDI.File.Load as Ml
import qualified Sound.MIDI.File as Mf

q  = '"'
bs = '\\'
    
replace :: Char -> String -> Char -> String
replace c s = let f x
                          | x == c    = s
                          | otherwise = [x]
              in f

escape s = s >>=
    replace bs [bs,bs] >>=
    replace q [bs,q] >>=
    replace '\n' [bs,bs,'n']

quote :: String -> String
quote s = q:s ++ [q]

-- make s appear in the LaTeX document
latex s = "putStrLn " ++ (quote . escape) s ++ "\n"

-- let ghci run s
ghci s = s ++ "\n"

-- \usepackage{fancyverb}
-- lhs2TeX messes with verbatim, but not with Verbatim
verbatim s =
    (latex $ '{':bs:"color[rgb]{0,0,0.5}") ++
    (latex $ bs:"begin{Verbatim}[frame=leftline]") ++
    concat s ++
    (latex $ bs:"end{Verbatim}") ++
    (latex "}") 
-- verbatim s = concat s



    
-- in lhs call this e.g. as \perform {:cmd verb ":t foldr"}
verb :: String -> IO String
verb cmd = (return . concat) $ [
    verbatim [latex $ "Main*> " ++ cmd],
    ghci $ ":cmd redir " ++ quote("xfoo" ++ " " ++ cmd),
    verbatim [ghci $ "putStrLn xfoo"]
    -- latex $ bs:"smallskip",
--    verbatim [ghci cmd]
    ]

-- show the commands that ghci will execute
test ios = do
    x <- ios
    putStrLn x


-- pretty-print something
pretty a = do
    P.pPrintNoColor a


prettyL_old f l as = do
    P.pPrintNoColor (take f as)
    putStrLn ". . ."
    P.pPrintNoColor (drop (length as - l) as)

-- xxx :: IO()
-- pretty-print a list, showing the first f and the last l elements
prettyL f l as = let txt1  = P.pShowNoColor (L.take f as)
                     txt2  = P.pShowNoColor (L.drop (length as - l) as)
                     txt1' = T.take (fromIntegral (T.length txt1 - 3)) txt1
                     txt2' = T.pack "," `T.append` T.drop 1 txt2
                 in do
                        putStrLn (T.unpack txt1')
                        putStrLn " ... "
                        putStrLn (T.unpack txt2')
        

clean s = let noUtf = (<256) . Data.Char.ord
              bar c = case c of
                  '|' -> "||"
                  _   -> [c]
          in filter noUtf s >>= bar
    
redir varcmd = case break Data.Char.isSpace varcmd of
                   (var,_:cmd) -> return $ unlines [
                       ":set -fno-print-bind-result",
                       "tmp <- System.Directory.getTemporaryDirectory",
                       "(f,h) <- System.IO.openTempFile tmp \"ghci\"",
                       "sto <- GHC.IO.Handle.hDuplicate System.IO.stdout",
                       "GHC.IO.Handle.hDuplicateTo h System.IO.stdout",
                       "System.IO.hClose h",
                       cmd,
                       "GHC.IO.Handle.hDuplicateTo sto System.IO.stdout",
                       "let readFileNow f = readFile f >>= \\t->length t `seq` return (clean t)",
                       var ++ " <- readFileNow f",
                       "System.Directory.removeFile f"
                       ]
                   _ -> return "putStrLn \"usage: :redir <var> <cmd>\"" 


