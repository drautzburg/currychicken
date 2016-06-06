include ../texmakefile

heap:
	ghc -O2 -prof -auto-all -caf-all -fforce-recomp des-example.hs -rtsopts
	des-example +RTS -hc -p
	hp2ps -c -e8in des-example.hp	
	okular des-example.ps &

prof:
	ghc -O2 -prof -auto-all -caf-all -fforce-recomp des-example.hs 
	des-example +RTS -p -RTS


loc.tex: *.lhs *.hs
	(echo '\\begin{Verbatim}[fontsize=\\footnotesize]'; cloc --by-file --quiet `ls *.lhs *.hs | grep -v xxx`; echo '\\end{Verbatim}') > loc.tex

examplerun:
	ghc -O2  -fforce-recomp des-example.hs 
	des-example

doc:
	cabal haddock --hyperlink-source

Insite.pdf: Insite.tex Des.tex Time.tex Logger.tex Logger.eps Handler.tex loc.tex
	pdflatex Insite.tex 




