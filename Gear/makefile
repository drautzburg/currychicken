include ../texmakefile


prof:
	ghc -O2 -prof -auto-all -caf-all -fforce-recomp des-example.hs 
	des-example +RTS -p -RTS


loc.tex: *.lhs *.hs
	(echo '\\begin{Verbatim}[fontsize=\\footnotesize]'; cloc --by-file --quiet `ls *.lhs *.hs | grep -v xxx`; echo '\\end{Verbatim}') > loc.tex

doc:
	cabal haddock --hyperlink-source

xxx: doc/Gear.pdf

$(DOC)Gear.pdf: $(DOC)Gear.tex $(DOC)playChords.tex $(DOC)render.tex $(DOC)renderGuitar.tex CommonFormatting.hs
	cd $(DOC) && pdflatex Gear.tex 




