import System.FilePath.Glob
import Control.Monad.State
import Control.Applicative
import Data.List

filePattern="*.log"
fileDirectory = "."

type LineNo = Int
data LineType = Error | Drop | Harmless
type Nline = (LineNo, String)
type DropLoc = Nline

-- | Do the actual work: check each line and add a message to the result
processLine :: Nline -> (Maybe DropLoc, [String]) -> (Maybe DropLoc, [String])
processLine nline (dl, msgs) = 
        case classifyLine (snd nline) of
            Harmless -> (dl, msgs)
            Drop     -> (Just nline, msgs)
            Error    -> case dl of
                            Nothing    -> (dl, (show nline):msgs)
                            Just (n,l) -> if n > (fst nline) - 3 
                                          then (dl, msgs) -- drop error
                                          else (dl, (show nline):msgs)
        where
            classifyLine :: String -> LineType
            classifyLine l = Error


-- | Invoke processLine for each line
processFile :: (FilePath, [Nline]) -> [String]
processFile (path, linesNumbered) = 
        let msgs = (snd .  foldr processLine (Nothing,[])) linesNumbered 
            result = map ((path ++ ": ") ++) msgs
        in "\n":result


-- |Find matching filename and process the contents of each of them
main = do
    matchingFiles <- fmap (concat . fst) $ globDir [compile filePattern] fileDirectory
    linesNumbered <- mapM nameAndContent matchingFiles 
    result  <- mapM (return . processFile) linesNumbered
    mapM_ putStrLn $ concat  result
            where
                nameAndContent :: FilePath -> IO (FilePath, [Nline])
                nameAndContent fn = do
                    content <- fmap lines $ readFile fn
                    return (fn, numberLines content)
                numberLines lines = zip [1..] lines

