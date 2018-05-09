\documentclass[a4paper]{article}
%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\usepackage{tikz}
\usetikzlibrary{arrows,mindmap,backgrounds}
\usepackage{verbatim}
\newenvironment{code}{\normalsize\verbatim}{\endverbatim\normalsize}
\usepackage{fancyhdr}
\usepackage{needspace}
\usepackage{amsmath}
\usepackage{graphicx}
\usepackage{capt-of}
\usepackage{ragged2e}
\usepackage[parfill]{parskip}
\usepackage{ctable}
\usepackage{color}
\usepackage{colortbl}	
\usepackage{pgf}
\usepackage[framemethod=tikz]{mdframed}
\newmdenv[frametitle=Running it,backgroundcolor=gray!05,roundcorner=2pt]{run}

% --------------
% tikz
% --------------
%\usepackage{tikz}
%\usetikzlibrary{calc} 
%\usetikzlibrary{decorations}
%\usetikzlibrary{plotmarks}
%\usetikzlibrary{arrows}
%\usetikzlibrary{chains,fit,shapes}

\usepackage{hyperref}
\hypersetup{colorlinks=true, linkcolor=blue, pdftoolbar=true}
\author{Martin Drautzburg}
\title{XXX}
\begin{document} \maketitle 

\begin{abstract}
\end{abstract}


\tableofcontents 
\listoffigures

\section{Introduction}

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}
\begin{code}
import Text.ParserCombinators.Parsec
import Text.Parsec.String
import Control.Monad
import System.Exit
import Data.Char
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

type CharPred = Char -> Bool
type CharSubst = Char -> [Char]

-- | Replace characters c satisfying p by fr c
subst :: CharPred -> CharSubst -> CharSubst
subst p fr c
    | p c       = fr c
    | otherwise = [c]

-- Character predicates
anyOf :: [Char] -> CharPred
anyOf = flip elem
fancyUtf :: CharPred
fancyUtf = (>=256) . Data.Char.ord

-- Character substitutions
byDoubling,byBackslash,byNothing :: CharSubst
byDoubling c    = [c,c]
byBackslash c = [bs,c]
byNothing c    = []


quote :: String -> String
quote s = let s' = s >>= subst (anyOf [qq,bs]) byBackslash
          in concat [[qq], s', [qq]]

\end{code}

\begin{code}
parseFile :: Parser a -> String -> IO a
parseFile p fileName = parseFromFile p fileName >>= either report return
  where
    report err = do
        hPutStrLn stderr $ "Error: " ++ show err
        exitFailure

run :: Parser ([IO()]) -> String -> IO()
run p s = case parse p "" s of
    Right ios -> runIos ios
    Left err -> putStrLn (show err)
\end{code}

\begin{code}
perform :: Parser (IO())
perform = let wrap = between (string "\\perform{") (string "}")
          in do
                cmd <- (wrap . many . noneOf) "}"
                (return . putStrLn) ("Performing " ++ cmd)

anythingElse :: Parser (IO())
anythingElse = do
    c <- anyChar
    (return . putChar) c

bar :: Parser [(IO())]
bar = many $ try perform <|> anythingElse

runIos [] = return ()
runIos ios = foldr1 (>>) ios

main = let filename = "playChords.lhs"
       in do
                putStrLn $ "parsing " ++ filename
                parseFile bar "playChords.lhs"

\end{code}

\end{document}
