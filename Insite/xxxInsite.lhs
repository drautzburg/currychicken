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
\usepackage{fancyvrb}
\usepackage[framemethod=tikz]{mdframed}
\newmdenv[
  frametitle=Output (intepreted):,
  backgroundcolor=white,
  topline=false,
  bottomline=false,
  skipabove={1em},
  skipbelow={1em},
  needspace=6em
]{run}


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
\title{Insite}
\begin{document} \maketitle 

\begin{abstract}
A Framework for creating discrete-event-simulations for logistic
networks
\end{abstract}


\tableofcontents 
\listoffigures

\section{Introduction}
A discrete-event-simulation computes a final |state| from an initial
|state|. In the context of a logistic network, the final state is not
that interesting. What's more interesting is whether or not something
remarkable happened along the way. 

Take for example a road simulation. The final state would tell you
where all the cars are, but it would not tell you whether or not there
was a traffic jam.

The state of the system we're simulating is called the |Domain|. 

Our concept is based on |Handlers| and |Loggers|. A Handler responds to
an |Event| and may change the Domain and schedule additional events. A
Logger has no effect on the Simulartion as such, but only logs
``interesting'' findings.

The state of the Simulation is bigger than the Domain-state, because
both Handlers and Loggers may carry their own states-
\section{Simulation}
\input{Time}
\input{Handler}
\input{Logger}
\input{Des}

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\begin{code}
x=42
\end{code}
\section{Appendices}
\needspace{20em}
\subsection{Lines of code}
\input{loc}
\end{document}

