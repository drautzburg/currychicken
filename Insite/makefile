include ../texmakefile

heap:
	ghc -O2 -prof -auto-all -caf-all -fforce-recomp des-example.hs -rtsopts
	des-example +RTS -hc -p
	hp2ps -c -e8in des-example.hp	
	okular des-example.ps &

thunk:
	ghc -O2 -prof -auto-all -caf-all -fforce-recomp des-example.hs 
	des-example +RTS -h -RTS
	hp2ps -c -e8in des-example.hp	
	okular des-example.ps &


examplerun:
	ghc -O2  -fforce-recomp example.hs 
	example

doc:
	cabal haddock --hyperlink-source





