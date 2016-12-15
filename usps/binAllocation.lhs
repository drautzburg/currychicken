\documentclass[a4paper]{article}
%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\usepackage{tikz}
\usetikzlibrary{arrows,mindmap,backgrounds}
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
\\newmdenv[frametitle=Running it,backgroundcolor=gray!05,roundcorner=2pt]{run}

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
\\author{Martin Drautzburg}
\\title{XXX}
\\begin{document} \maketitle 

\\begin{abstract}
\end{abstract}


\\tableofcontents 
\listoffigures

\section{Introduction}

%\\begin{figure}[htb!]
%\\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\begin{code}
import Debug.Trace

type Price = Double
type Name = String
type Volume = Int


data Bin = Bin Name Price Volume
                  deriving (Eq, Show)

binName (Bin n _ _) = n

binNames bins = map binName bins


data Separation = SepNode Name Price [Separation] |
                  SepLeaf Name Price Volume
                  deriving (Eq, Show)

sepName (SepNode n _ _) = n
sepName (SepLeaf n _ _) = n

sepAccCost :: Separation -> Price
sepAccCost (SepNode _ p _) = p
sepAccCost (SepLeaf _ p _) = p

sepVolume :: Separation -> Volume
sepVolume (SepLeaf _ _ v) = v
sepVolume (SepNode _ _ ss) = sum (map sepVolume ss)

sepRemove :: Name -> [Separation] -> [Separation]
sepRemove _ [] = []
sepRemove xsep (sep : seps) =
  case sep of
    (SepNode n p children) -> if (n==xsep) then sepRemove xsep seps
                              else SepNode n p (sepRemove xn children) : sepRemove xn seps
    (SepLeaf n p v) ->if (n==xsep) then sepRemove xn seps
                      else (SepLeaf n p v) : sepRemove xn seps


sepNames :: [Separation] -> [Name]
sepNames [] = []
sepNames (sep:seps) =
  case sep of
    (SepNode n _ []) -> n : sepNames seps
    (SepNode n _ cn) -> (n : sepNames cn) ++ sepNames seps
    (SepLeaf n _ _) -> n : sepNames seps
    

ex_separations = [
  SepNode "801" 20.0 [
        SepLeaf "801a" 10.0 10000,
        SepLeaf "801b" 10.0 10000
                ],
  SepNode "802" 20.0 [
        SepLeaf "802a" 10.0 10000,
        SepLeaf "802b" 10.0 10000
                ]
  ]

ex_bins = [
  Bin "bin1" 10.0 20000,
  Bin "bin2" 10.0 20000
  ]

binCost :: (Separation, Bin) -> Price
binCost (sep, Bin _ p v) = let sweepCost = fromIntegral(sepVolume sep `div` v) * p
                               acceptanceCost = (fromIntegral $ sepVolume sep) * sepAccCost sep
                           in sweepCost + acceptanceCost


{-
ex_alloc :: [Bin] -> [Separation] ->  [[(Name, Name)]]

ex_alloc (bin:[]) seps =
  [[(binName bin, sepName s) | s<-seps]]

ex_alloc [] _ = []
ex_alloc (bin:bins) seps =
  let a = [(binName bin, sep) | sep<-sepNames seps]
  in do
    h <- a
    map (h:) ( ex_alloc bins (sepRemove (snd a) seps))



showAllocs :: [[(Name, Name)]] -> [String]
showAllocs allocs = map show allocs

printAllocs :: [[(Name, Name)]] -> IO()
printAllocs allocs = mapM_  putStrLn (showAllocs allocs)

xxx = ex_alloc ex_bins ex_separations
-}

xx1 = [1,2]
xx2 = [10,20,30]

xxf :: [Integer] -> [Integer] -> [[(Integer,Integer)]]
xxf [] _ = [[]]
xxf (a:as) bs = do
  b <- bs
  let rest = filter (/=b) bs
  x <- xxf as rest
  return $ (a,b) : x


-- xxf x y = trace ((show x) ++ " - " ++ (show y)) []
\end{code}

\end{document}
