\documentclass[a4paper]{article}
%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts

\usepackage{tikz}
\usetikzlibrary{arrows,mindmap,backgrounds}
\usepackage{fancyhdr}
\pagestyle{fancy}
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
\usepackage{pifont}


\usepackage{thmbox}
%\newtheorem[S]{rule}{Rule}
\newtheorem[S]{note}{Note}

\usepackage[all]{nowidow}
\usepackage{ragged2e}
\RaggedRight

\usepackage[framemethod=tikz]{mdframed}
\newmdenv[frametitle=Running it,backgroundcolor=gray!05,roundcorner=2pt]{run}

\hyphenation{work-order}

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

\title{Opal Concept}
\begin{document} \maketitle 

\begin{abstract}
\end{abstract}

\begin{center}
\includegraphics[width=3cm]{../AtTheMovies.jpg}
\end{center}

\tableofcontents 
\listoffigures

% Start each section on a new page from here on
\let\stdsection\section
\renewcommand\section{\newpage\stdsection}

%------------------------------------------------------------
\section{Goals}
%------------------------------------------------------------
The Goals of Opal can be summarized as follows

\subsection{Find optimal Production Plans}
\begin{itemize}
\item Minimize violations of Network-Planning input (e.g. bookings)
\item Minimize resource usage
\item Meet constraints
  \begin{itemize}
  \item do not use unavailable resources (personnel, machines,
    sortplans)
  \item don't send material to Processes which are incapable of
    processing it (don't send parcels to a letter sorting machine)
  \end{itemize}
\item Respond to a changing environment (allow easy re-planning)
\end{itemize}

\subsection{Report characteristics of Production Plans}
\begin{itemize}
\item Report the Production Plan itself, e.g. in the form of a
  timeline
\item Report the resource demands and the resource assignments
\item Report other ``bad things'' e.g. waiting times, excess resources
\item Report violations of Network-Planning input, particularly SLA
  violations
\item Report how a production plan changed over time, particularly
  planned vs. actual
\item Provide input to other planning systems, particularly Sorting
  Planning, Resource Planning and Network Planning (send re-bookings)
\end{itemize}
\subsection{Execute Production Plan}
\begin{itemize}
\item Communicate with external systems (``avoid the megaphone'')
\end{itemize}

%------------------------------------------------------------
\section{The World}
%------------------------------------------------------------

The |World| stands for what Opal needs to know about the world. This
includes:
\begin{description}
\item[Material State] This is the position of each and every item Opal
  cares about
\item[Resource State] This is the current occupation and position
  of each and every resource.
\item[Workorder State] This wether or not Workorders are active and
  what they are doing.
\item[Time] This is the current time of the world
\end{description}

%------------------------------------------------------------
\input{items}
\input{monitoring}
\input{workorders} 
\input{productionBatch}
\input{whatOpalDoes}
%------------------------------------------------------------

\end{document}
