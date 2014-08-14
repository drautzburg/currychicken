
import System.FilePath.Glob
import Control.Monad.State
import Control.Applicative
import Data.List
import Text.Regex.Posix
import Debug.Trace

filePattern="create_extra*.log"
fileDirectory = "J:/tracelog/rdb/PPS/PPS"

type LineNo = Int
data LineType = Error | Drop | Harmless
type NumberedLine = (LineNo, String)
type DropLoc = NumberedLine

-- | Do the actual work: check each line and add a message to the result
processLine :: NumberedLine -> (Maybe DropLoc, [NumberedLine]) -> (Maybe DropLoc, [NumberedLine])
processLine numberedLine (dl, msgs) =
        case classifyLine (snd numberedLine) of
            Harmless -> (dl, msgs)
            Drop     -> (trace "-- drop --" Just numberedLine, msgs)
            Error    -> case trace (show dl) dl of
                            Nothing    -> (dl, numberedLine:msgs)
                            Just (n,l) -> if n > (fst numberedLine) - 10
                                          then (dl, msgs) -- drop error
                                          else (dl, numberedLine:msgs)
  where
    classifyLine l 
      | l=~ "ORA-" = Error
      | l=~ "DROP" = trace ("-->"++l) Drop
      | otherwise  = Harmless


-- | Invoke processLine for each line
processFile :: (FilePath, [NumberedLine]) -> (FilePath,[NumberedLine])
processFile (path, linesNumbered) =
        let msgs = (snd .  foldr processLine (Nothing,[])) linesNumbered
            result = (path, msgs)
        in result


-- |Find matching filename and process the contents of each of them

main = do
  putStrLn fileDirectory
  matchingFiles <- fmap (concat . fst) $ globDir [compile filePattern] fileDirectory
  linesNumbered <- mapM nameAndContent matchingFiles
  result  <- mapM (return . processFile) linesNumbered
  mapM_ putStrLn $ concatMap showResult result
    where
      nameAndContent :: FilePath -> IO (FilePath, [NumberedLine])
      nameAndContent fn = do
        content <- fmap lines $ readFile fn
        return (fn, numberLines content)
      numberLines lines = zip [1..] lines
      bareName path = let bn = stripPrefix fileDirectory path
                      in case bn of
                        Nothing -> path
                        Just b  -> "..." ++ b
      showResult (path, nls) = map (\l -> (bareName path) ++ " [" ++ (show $ fst l) ++ "] " ++ (snd l)) nls

