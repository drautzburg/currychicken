
%.tex: %.lhs
	lhs2TeX --tt $? > $(?:.lhs=.tex)

%.pdf: %.tex
	pdflatex $<

%.eps: %.dot
	dot -Teps $? > $(?:.dot=.eps)

%: %.lhs
	ghc -O2 --make -rtsopts $?

all: modularSimulation.pdf

clean: 
	rm modularSimulation.tex modularSimulation.pdf


modularSimulation.pdf: modularSimulation.tex modularSimulation0.eps modularSimulation1.eps modularSimulation2.eps modularSimulation3.eps modularSimulation4.eps

test: modularSimulation
	time modularSimulation # +RTS -sstderr


