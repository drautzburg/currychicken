------------------------------------------------------------
-- Functions for capturing ghci output to be used with lhs2TeX
------------------------------------------------------------
module CommonFormatting (
    pretty,
    prettyL,
    verb,
    ctors,
    redirOpen,
    redirClose,
    forLhs2TeX,
    test,
    fmtNone,
    fmtVerbatim,
    fmtPrompt,
    fmtCtors
    )
where
import Sound.MIDI.File
import Debug.Trace
import Data.List
import qualified Data.Char
import qualified Text.Pretty.Simple as P
import qualified Data.Text.Lazy as T
import qualified Data.List as L
import qualified Sound.MIDI.File.Load as Ml
import qualified Sound.MIDI.File as Mf
import System.Directory
import System.IO
import GHC.IO.Handle
------------------------------------------------------------
-- quoting, escaping and filtering
------------------------------------------------------------
qq  = '"'
bs = '\\'
nl = '\n'
    
replace :: Char -> String -> Char -> String
replace c s = let f x
                          | x == c    = s
                          | otherwise = [x]
              in f

-- Character predicates
type CharPred = Char -> Bool
anyOf :: [Char] -> CharPred
anyOf = flip elem
fancyUtf :: CharPred
fancyUtf = (>=256) . Data.Char.ord

-- Character substitutions
type CharSubst = Char -> [Char]
byDoubling,byBackslash,byNothing :: CharSubst
byDoubling c    = [c,c]
byBackslash c = [bs,c]
byNothing c    = []

-- | Replace characters c satisfying p by fr c
subst :: CharPred -> CharSubst -> CharSubst
subst p fr c
    | p c       = fr c
    | otherwise = [c]


escape s = s 
           >>= subst (anyOf [bs,qq]) byBackslash 

-- | process output for lhs2TeX
forLhs2TeX s = s >>=
          subst fancyUtf byNothing >>=
          subst (anyOf "|@") byDoubling

quote :: String -> String
quote s = concat [[qq], s, [qq]]

chomp = dropWhileEnd ((==) '\n')
------------------------------------------------------------
-- GHCI interaction
------------------------------------------------------------

-- When entering ":cmd foo" in ghci, ghci will evaluate the function
-- foo and treats its result as ghci commands and executes them. E.g. typing
-- :cmd (return ":t foldr") will print
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- \usepackage{fancyverb}
-- lhs2TeX messes with verbatim, but not with Verbatim


-- result formatting
fmtNone, fmtVerbatim, fmtPrompt, fmtCtors :: String -> IO ()

fmtNone s = return ()
fmtVerbatim = putStrLn . verbatim . forLhs2TeX
    where
        verbatim s = unlines [
            '{':bs:"color[rgb]{0,0,0.5}",
            bs:"begin{Verbatim}[frame=leftline]",
            chomp s,
            bs:"end{Verbatim}",
            "}"
            ]

fmtPrompt cmd = fmtVerbatim ("Main*> " ++ cmd)
fmtCtors = fmtVerbatim . unlines . filter (startsWith ["type","data","newtype"]) . lines
    where
        startsWith :: [String] -> String -> Bool
        startsWith inits s = foldr (\i b -> b || isPrefixOf i s) False inits

-- in lhs call this e.g. as \perform {:cmd verbTo "fmtPrompt" "fmtVerbatim" ":t foldr"}
-- where takeAll :: String -> IO ()
verbTo :: String -> String -> String -> IO String
verbTo fmtCmd fmtResult  cmd = (return . unlines) $ [
    unwords [fmtCmd, quote(cmd)],
    ":set -fno-print-explicit-foralls",
    ":set -fno-print-bind-result",
    "(sto,f) <- redirOpen",
    cmd,
    unwords ["redirClose sto f >>=" , fmtResult]
    ]


verb, ctors :: String -> IO String

-- insert output: \perform {:cmd verb ":t foldr"}
verb = verbTo "fmtPrompt" "fmtVerbatim"

-- insert constructors: \perform {:cmd ctors "[]"}
ctors aType = verbTo "fmtNone" "fmtCtors" (":info " ++ aType)


redirOpen = do
    tmp <- System.Directory.getTemporaryDirectory
    (f,h) <- System.IO.openTempFile tmp "redir"
    sto <- GHC.IO.Handle.hDuplicate System.IO.stdout
    GHC.IO.Handle.hDuplicateTo h System.IO.stdout
    System.IO.hClose h
    return (sto,f)

redirClose :: Handle -> FilePath -> IO String
redirClose sto f = do
    GHC.IO.Handle.hDuplicateTo sto System.IO.stdout
    result <- readFile f >>= \t->length t `seq` return t
    System.Directory.removeFile f
    return result


    

-- show the commands that ghci will execute
test ios = do
    x <- ios
    putStrLn x

---------------------------------------------------------------------
-- General pretty printing
---------------------------------------------------------------------
-- pretty-print something
pretty a = do
    P.pPrintNoColor a


prettyL_old f l as = do
    P.pPrintNoColor (take f as)
    putStrLn ". . ."
    P.pPrintNoColor (drop (length as - l) as)

-- pretty-print a list, showing the first f and the last l elements
prettyL f l as = let txt1  = P.pShowNoColor (L.take f as)
                     txt2  = P.pShowNoColor (L.drop (length as - l) as)
                     txt1' = T.take (fromIntegral (T.length txt1 - 3)) txt1
                     txt2' = T.pack "," `T.append` T.drop 1 txt2
                 in do
                        putStrLn (T.unpack txt1')
                        putStrLn " ... "
                        putStrLn (T.unpack txt2')
        


{-
-- :cmd redir "foo :info foldr"
-- will capture the result of ":info foldr" in the variable foo
-- you could print it using "putStrLn foo"

redir :: Monad m => String -> m String    
redir varcmd = case break Data.Char.isSpace varcmd of
                    (var,_:cmd) -> return $ unlines [
                        ":set -fno-print-bind-result",
                        "(sto,f) <- redirOpen",
                        cmd,
                        var ++ "<- redirClose sto f"
                                                   ]
                    _ -> return "putStrLn \"usage: :redir <var> <cmd>\"" 
-}

-- :cmd redir "foo :info foldr" "foo"
-- will pass the result of ":info foldr" to the function foo :: String -> IO String

