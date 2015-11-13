include ../texmakefile

prof:
	ghc -O2 -prof -auto-all -caf-all -fforce-recomp des-example.hs -rtsopts
	des-example +RTS -hc -p
	hp2ps -e8in des-example.hp	
	okular des-example.ps &


examplerun:
	ghc -O2  -fforce-recomp example.hs 
	example

doc:
	cabal haddock --hyperlink-source





