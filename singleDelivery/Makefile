.SUFFIXES: .lhs .tex

SOURCES=Main.hs SingleDelivery.lhs Mail.lhs Plot.lhs

all: SingleDelivery.pdf

SingleDelivery.pdf: SingleDelivery.tex Mail.tex Plot.tex MailTest.tex
	pdflatex  SingleDelivery.tex 

plots: plotXyQ.tex plotVanillaQ.tex

plotXyQ.tex: SingleDelivery.lhs Plot.lhs
	runFunction.sh plotXyQ
	ghc --make -o runFunction runFunction.hs
	runFunction

plotVanillaQ.tex: SingleDelivery.lhs Plot.lhs
	runFunction.sh plotVanillaQ
	ghc --make -o runFunction runFunction.hs
	runFunction


.lhs.tex:
#	lhs2TeX --tt $< > $@
	lhs2TeX --poly $< > $@

clean:
	rm *tex *pdf *.o

main: $(SOURCES)
	ghc -rtsopts -prof -auto-all  --make -o main Main.hs

profile: 
	ghc -O2 -fforce-recomp -rtsopts -prof -auto-all -caf-all  --make -o main Main.hs
	main  1e6 +RTS -p -sstderr 
#	hp2ps main.hp

