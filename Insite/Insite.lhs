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
\newmdenv[
  frametitle=Output:,
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
\title{XXX}
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
remarkable happened along the way. Take for example a road
simulation. The final state would tell you where all the cars are, but
it would not tell you whether or not there was a traffic jam.

The |state| of the simulation is described by 
\begin{description}
\item[A Domain,] which repensts the state of the system we're simulating. 
\item[A Log,] which accumulates log entries. The initial log may be
  empty, but does not have to.
\item[An Event-queue.] The initial Event-queue holds events, which are
  known to happen in the future, but can also be empty.
\end{description}
      
The way the state is altered is described by the |behavior| of the
simulation. The behavior consist of three components:
\begin{description}
\item[A Logger,] which can produce new log entries whenever an Event
  was handled.
\item[A Handler,] which receives an Event and the current Domain and
  produces a new Domain and possibly schedules additional Events.
\item[An exit condition,] which returns true when the simulation shall
  end.
\end{description}
\input{Des}

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\begin{code}
x=42
\end{code}
\end{document}
