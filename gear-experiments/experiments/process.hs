import System.Process
import GHC.IO.Handle
import CommonFormatting
xxx = do
      (Just hin, Just hout, _, _) <- createProcess (proc "/bin/cat" []){ std_out = CreatePipe, std_in = CreatePipe } 
      hPutStr hin "xxx\n"
      hPutStr hin "yyy\n"
      putStrLn "1"
      x <- hGetLine hout
      putStrLn x
    
