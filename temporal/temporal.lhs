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
import Data.List.Ordered
import Control.Applicative
import Data.Function

type Time = Integer 
data Event a = Evt {
            t :: Time,
            v :: a
        } deriving (Show)


-- apply a function to the value
vFunc :: (a->b) -> Event a -> Event b
vFunc f e = Evt (t e) (f $ v e)

-- apply a function to the time
tFunc :: (Time->Time) -> Event a -> Event a
tFunc f e = Evt (f $ t e) (v e)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

data Temporal a = Temporal a [Event a] 
                  deriving (Show)

tConst :: Temporal a -> a
tConst (Temporal xc xs) = xc

tEvents :: Temporal a -> [Event a]
tEvents (Temporal xc xs) = xs

-- get event data including the default constant
tEventData :: Temporal a -> [a]
tEventData (Temporal c es) = c : (map v es)

tAlign :: [Event a] -> [Event b] -> (a,b) -> ([Event (a,b)])
tAlign exs [] (x,y) = map (vFunc (\xi -> (xi,y))) exs
tAlign [] eys (x,y) = map (vFunc (\yi -> (x,yi))) eys
tAlign ex@((Evt tx xi):exs) ey@((Evt ty yi):eys) (x,y)
        | tx == ty = Evt ty (xi,yi) : tAlign exs eys (xi,yi)
        | tx <  ty = Evt tx (xi, y) : tAlign exs ey  (xi, y)
        | tx >  ty = Evt ty (x, yi) : tAlign ex  eys (x, yi)


-- limit temporal to t1 <= te < t2
tWindow :: (Time, Time) -> Temporal a -> Temporal a
tWindow (t1, t2) (Temporal xc xs) = Temporal c $ takeWhile ((< t2) . t) late
        where
            (c,late)  = dropwhile ((<= t1)  . t) xc xs

            -- like dropWhile but also return the final constant
            dropwhile  :: (Event a -> Bool) -> a -> [Event a] -> (a, [Event a])
            dropwhile _ c []    =  (c, [])
            dropwhile p c xs@(x:xs')
                    | p x       =  dropwhile p (v x) xs'
                    | otherwise =  (c,xs)

tAt :: Time -> Temporal a -> a
tAt t = tConst . (tWindow (t,t))

instance Functor Temporal where
        fmap f (Temporal xc xs) = Temporal (f xc) (map (vFunc f) xs)


instance Applicative Temporal where
        pure x = Temporal x []

        (Temporal fc fs) <*> (Temporal xc xs) = Temporal (fc xc) (map apply $ tAlign fs xs (fc,xc))
                where
                    apply (Evt t (f,x)) = Evt t (f x)

ext1 = Temporal 10 [(Evt 10 1),(Evt 20 2)]
ext2 = Temporal 20 [(Evt 15 1),(Evt 25 2), (Evt 35 3)]

ext3 = Temporal 0 (map (\x -> Evt x x) [1..100000])
-- take 10 $ tEventData $ (\x y z -> x*y*z) <$> ext3 <*> ext3 <*> ext3
-- sum $ tEventData $ (*) <$> ext3 <*> ext3

\end{code}



%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
