redir varcmd = case break Data.Char.isSpace varcmd of
    (var,_:cmd) -> return $ unlines [":set -fno-print-bind-result",
                                     "tmp   <- System.Directory.getTemporaryDirectory",
                                     "(f,h) <- System.IO.openTempFile tmp \"ghci\""
                                     "sto   <- GHC.Handle.hDuplicate System.IO.stdout"
                                     "GHC.Handle.hDuplicateTo h System.IO.stdout"
                                     "System.IO.hClose h",
                                     cmd,
                                     "GHC.Handle.hDuplicateTo sto System.IO.stdout",
                                     "let readFileNow f = readFile f >>= \\t->length t `seq` return t",
                                     var++" <- readFileNow f","System.Directory.removeFile f"
                                    ]
--               _           -> return "putStrLn \"usage: :redir <var> <cmd>\"" 
