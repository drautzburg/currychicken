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
\begin{code}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

import qualified Data.List as L
import Data.Function
import qualified Data.Map as M

import qualified Text.Show.Pretty as Pr
pp x = putStrLn $ Pr.ppShow x

type Column = String
type Value  = String


data DistRow      = Row {atts::[Value], chance:: Double} deriving (Eq,Show)

type Distribution = [DistRow]


dEmpty :: Distribution 
dEmpty = []


exDist1 :: Distribution 
exDist1 = dNorm [
 Row ["Prio", "Letter"] 0.2,
 Row ["Prio", "Flat"] 0.1,
 Row ["Ord", "Letter"] 0.4
 ]

exDist2 :: Distribution 
exDist2 = dNorm [
 Row ["Prio", "DE120"] 0.1,
 Row ["Prio", "DE121"] 0.2,
 Row ["Prio", "DE122"] 0.3,
 Row ["Prio", "DE123"] 0.4,
 Row ["Prio", "DE124"] 0.5,
 Row ["Ord", "DE120"] 0.6,
 Row ["Ord", "DE121"] 0.7,
 Row ["Ord", "DE122"] 0.8,
 Row ["Ord", "DE123"] 0.9,
 Row ["Ord", "DE124"] 0.1
 ]



rProject :: [Int] -> DistRow -> DistRow
rProject ixs (Row vs c) = Row  (map (vs !!) ixs) c

dProject :: [Int] -> Distribution -> Distribution
dProject ixs dist = (sumup.group.sort.project) dist
        where
            project  = map (rProject ixs)
            sort     = L.sortBy (compare `on` atts)
            group    = L.groupBy ((==) `on` atts)
            sumup    = map (foldr1 s)
            s r1 r2  = Row (atts r1) (chance r1 + chance r2)


dSum = sum . map chance
dMap f dist = map (\row -> row{chance = f (chance row)}) dist
dNorm dist = dMap (/dSum dist) dist


type Rcompare a = DistRow -> DistRow -> a

rCompareBy :: (Int, Int) -> Rcompare Ordering
rCompareBy (i1,i2) r1 r2  = compare (atts r1 !! i1) (atts r2 !! i2)

rCompareAllBy :: [(Int, Int)] -> Rcompare Ordering
rCompareAllBy = foldl f eq 
        where
            f cmp (x1,x2) = cmp <||> rCompareBy (x1,x2)
            (<||>) :: Rcompare Ordering -> Rcompare Ordering -> Rcompare Ordering
            c1 <||> c2 = \r1 r2 -> case c1 r1 r2 of
                                       LT -> LT
                                       GT -> GT
                                       EQ -> c2 r1 r2


rEqAllBy :: [(Int, Int)] -> Rcompare Bool
rEqAllBy ixs r1 r2 = (== EQ) $ rCompareAllBy ixs r1 r2

eq = const (const EQ)

dSortBy :: [Int] -> Distribution -> Distribution
dSortBy ixs = L.sortBy (rCompareAllBy (zip ixs ixs))


dExpand :: [(Int,Int)] -> Distribution -> Distribution -> Distribution
dExpand ixs d1 d2 = do
    r2 <- d2
    r1 <- dNorm $ filter (rEqAllBy ixs r2) d1
    return $ Row  (atts r1 ++ atts r2) (chance r1 * chance r2)

ex1 = pp $ dProject [0,1] $ dExpand [(0,0)] exDist1 exDist2



\end{code}


\begin{code}

\end{code}

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
