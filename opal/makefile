
export DFD_STORE
export EMPTY_LHS

opal.pdf: opal.tex items.tex monitoring.tex workorders.tex productionBatch.tex whatOpalDoes.tex \
	  interpolating.eps  unpackComplete.eps workorderStates.eps whatOpalDoes.eps stateDependencies.eps
	pdflatex opal.tex 


%.eps: %.dot
	dot -T eps -l dfd.ps $? > $@

%.jpeg: %.dot
	dot -T jpeg -l dfd.ps $? > $@

%.jpeg: %.dot
	dot -T jpeg -l dfd.ps $? > $@

%.pdf: %.eps
	ps2pdf 	-sPAPERSIZE=a3 $?

%.tex: %.lhs
	lhs2TeX --tt $? > $(@:.lhs=.tex)

%pdf: %tex
	pdflatex $?

%.view: %.pdf
	"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe" $?

dfd.ps:
	echo "$$DFD_STORE" > dfd.ps

%.lhs:
	echo "$$EMPTY_LHS" > $@

touch:
	touch *.lhs

clean:
	rm -f *.pdf  *.png *.tex *.toc *.aux *.hi *.lof *.o *.out *.prof *.log *~ 


Products.pdf: Products.lhs ProductsRestrict.eps ProductsMerge.eps  ProductsSplit.eps \
	ProductsSplitPack.eps ProductsPack.eps ProductsExRec.eps ProductsUnpack.eps
# ProductsCombine.eps

# ------------------------------------------------------------
# verbatim files
# ------------------------------------------------------------

# --- postscript file for dataflow store component (dfd.ps) ---

define DFD_STORE
/dfd_store {
        {
          4 ne { stop } if % sanity-check number of sides
          aload pop
          pop pop
          8 4 roll
          moveto
          lineto
          lineto
          lineto
          fill
        }
        {
          4 ne { stop } if % sanity-check number of sides
          aload pop
          pop pop
          8 4 roll
          moveto
          lineto
          moveto
          lineto
          stroke
        }
        ifelse
} bind def
endef

# --- empty lhs document ---

define EMPTY_LHS
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

%\begin{figure}[htb!]
%\centering
%\includegraphics[width=4cm]{glass-slipper.jpg}
%\end{figure}

\end{document}
endef
