include ../texmakefile

heap:
	ghc -O2 -prof -auto-all -caf-all -fforce-recomp des-example.hs -rtsopts
	des-example +RTS -hc -p
	hp2ps -c -e8in des-example.hp	
	okular des-example.ps &

prof:
	ghc -O2 -prof -auto-all -caf-all -fforce-recomp des-example.hs 
	des-example +RTS -p -RTS



examplerun:
	ghc -O2  -fforce-recomp des-example.hs 
	des-example

doc:
	cabal haddock --hyperlink-source





