%include lhs2TeX.fmt
%include colorcode.fmt
%include greek.fmt
\renewcommand{\hscodestyle}{\small}

\subsection{Plot}

This section contains functions for plotting simulation results. It
uses the \emph{Tikz} library for LaTeX. We put them into an extra file
and section to make the main document less noisy. 

\begin{figure}[htb!]
\centering
\begin{tikzpicture}[domain=0:10, scale=1, x=1cm, y=1cm]
\draw[very thin,color=gray, xstep=1cm, ystep=0.5cm] (0,90.9) grid (10,94);
\draw[thick, color=red] plot[smooth] (\x,{91.8}) node[right, yshift=-3pt]{$Q$};
\draw [very thick, color=blue](0, 92.7) circle (1pt);
\draw [very thick, color=blue](1, 91.5) circle (1pt);
\draw [very thick, color=blue](2, 91.4) circle (1pt);
\draw [very thick, color=blue](3, 91.3) circle (1pt);
\draw [very thick, color=blue](4, 91.9) circle (1pt);
\draw [very thick, color=blue](5, 91.3) circle (1pt);
\draw [very thick, color=blue](6, 91.6) circle (1pt);
\draw [very thick, color=blue](7, 91.5) circle (1pt);
\foreach \x in {1,2,3,4,5,6,7,8,9}
    \draw (\x,91) -- (\x,90.9) node[below] {$\x$};
\foreach \y in {91, 92, 93, 94}
    \draw (0,\y) -- (-0.1,\y) node[left] {\y\%};

\end{tikzpicture}
\caption{Example output of plotQ}
\end{figure}
 
\begin{code}
module Plot where
import System.IO
plotQ filename caption expected points = do
    h <- openFile filename WriteMode
    putStr ("writing " ++ filename ++ " ... ")
    hPutStrLn h "\\begin{figure}[htb!]"
    hPutStrLn h "\\centering"
    hPutStrLn h "\\begin{tikzpicture}[domain=0:10, scale=1, x=1cm, y=1cm]"

    -- plot the expected value
    hPutStrLn h (
                 "\\draw[thick, color=red] plot[smooth] (\\x,{"
                 ++ show expected
                 ++ "}) node[right, yshift=-3pt]{$Q_{calc}(1) = " ++ (show expected) ++ "$};"
                )
    -- plot the data
    plotData h (reverse points) 0

    -- plot the axes and the grid
    hPutStrLn h "\\draw[very thin,color=gray, xstep=1cm, ystep=0.5cm] (0,89.9) grid (10,93);"
    hPutStrLn h "\\foreach \\x in {1,2,3,4,5,6,7,8,9}"
    hPutStrLn h "    \\draw (\\x,90) -- (\\x,89.9) node[below] {$\\x$};"
    hPutStrLn h "\\foreach \\y in {90, 91, 92, 93}"
    hPutStrLn h "    \\draw (0,\\y) -- (-0.1,\\y) node[left] {\\y\\%};"
              
    hPutStrLn h "\\end{tikzpicture}"
    hPutStrLn h ("\\caption{" ++ caption ++ "}")
    hPutStrLn h "\\end{figure}"

    hClose h
    putStrLn "done"
    return ()
        where
            plotData _ [] _ = do
                return ()
            plotData h (p:ps) n= do
                hPutStrLn h (
                             "    \\draw [very thick, color=blue]("
                             ++ (show n) ++ "," ++ (show $ 100*p)
                             ++ ") circle (1pt);"
                            )
                plotData h ps (n+1)
                

\end{code}

\begin{code}
-- wrap thing
plotTikz caption content =
    "\\begin{figure}[htb!]"
    ++ "\\centering"
    ++ "\\begin{tikzpicture}[domain=0:10, scale=1, x=1cm, y=1cm]"
    ++ "\\end{tikzpicture}"
    ++ "\\caption{" ++ caption ++ "}"
    ++ "\\end{figure}"
\end{code}
