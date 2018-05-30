\documentclass[a4paper]{article}
%include lhs2TeX.fmt
%include greek.fmt
%options ghci -fglasgow-exts
\usepackage{fancyvrb}
\usepackage{tikz}
\usetikzlibrary{arrows,mindmap,backgrounds}
\usepackage{fancyhdr}
\usepackage{listing}
\usepackage{needspace}
\usepackage{amsmath}
\usepackage{graphicx}
\usepackage{capt-of}
\usepackage{ragged2e}
\usepackage[parfill]{parskip}
\usepackage{ctable}
\usepackage{xcolor}
\usepackage{colortbl}	
\usepackage{pgf}
\usepackage[framemethod=tikz]{mdframed}
\newmdenv[frametitle=Example,skipabove=1em,backgroundcolor=gray!05,roundcorner=2pt]{run}

% --------------
% tikz
% --------------
%\usepackage{tikz}
%\usetikzlibrary{calc} 
%\usetikzlibrary{decorations}
%\usetikzlibrary{plotmarks}
%\usetikzlibrary{arrows}
%\usetikzlibrary{chains,fit,shapes}

\usepackage[utf8]{inputenc}

\usepackage{hyperref}


\hypersetup{colorlinks=true, linkcolor=blue, pdftoolbar=true}
\author{Martin Drautzburg}
\title{Gear\\
Music composition toolkit
}

\begin{document} \maketitle 

\begin{abstract}
\end{abstract}


\tableofcontents 
\listoffigures

\section{Introduction}
Like any other program, a program for composing music needs to produce
some output. This can be Midi data to be played on Midi devices, or
score output to be printed on paper.

Gear attempts to make it as easy as possible to produce the desired
output. It uses Haskell as the language for composing music, such that
writing music is achieved by writing a Haskell program. After all,
Haskell is often praised as a very ``compositional'' programming
language.

In order to design the API offered by Gear, we do not start with a
definition of ``what is music'' but with ``musical intent''. Turning
musical intent into reality is the same as solving a composition
problem. So we start by presenting some of the problems, a composer
might have to face. From there, we derive suitable abstractions.

\section{Requirements}
In this paragraph, we present composition problems we want to solve
with Gear.
\input{playChords}
\input{render}
\input{renderMidi}

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%nd{figure}
\end{document}
