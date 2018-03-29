import Control.Monad (void, ap)
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

import System.IO
import System.Exit


verb s = "\\begin{verbatim}\n" ++ s ++ "\\end{verbatim}\n"

ps :: Parser a -> String -> Either ParseError a
ps p = parse p ""

parseFile :: Parser a -> String -> IO a
parseFile p fileName = do
    result <- parseFromFile p fileName
    either report return result

report err = do
    hPutStrLn stderr $ "Error: " ++ show err
    exitFailure

latexBlock :: Parser String
latexBlock = do
    latex <- manyTill anyChar (try codeBlock)
    codeBlock
    eof
    return latex
    

codeBlock :: Parser String
codeBlock = do
    string "\\begin{code}\n" 
    code <- manyTill anyChar (string "\\end{code}\n")
    return $ verb code



xxx :: Parser String
xxx = do
    xs <- latexBlock
    ys <- codeBlock
    eof
    return (xs++ys)

