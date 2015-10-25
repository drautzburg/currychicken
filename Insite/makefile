
prof:
	ghc -O2 -prof -auto-all -caf-all -fforce-recomp example.hs -rtsopts
	example +RTS -hc -p
	hp2ps -e8in example.hp	
	okular example.ps &
#	example +RTS -p
#	ee example.prof

examplerun:
	ghc -O2  -fforce-recomp example.hs 
	example

clean:
	for i in '*.html' '*.js' '*.css' '*.hi' '*.o' '*.aux' '*.prof' '*.hp'	; do find . -name $i; done;




