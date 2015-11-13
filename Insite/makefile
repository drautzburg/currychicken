include ../texmakefile

prof:
	ghc -O2 -prof -auto-all -caf-all -fforce-recomp example.hs -rtsopts
	example +RTS -hc -p
	hp2ps -e8in example.hp	
	okular example.ps &


examplerun:
	ghc -O2  -fforce-recomp example.hs 
	example

doc:
	cabal haddock --hyperlink-source





