include ../texmakefile

Temporal.pdf: Temporal.lhs tJoin.png align.png 

%.dyn_o: %.hs
	ghc -dynamic -shared $? -o $@

%.dyn_o: %.lhs
	ghc -dynamic -shared $? -o $@

examples: Temporal.lhs TemporalExamples.hs
	ghc --make TemporalExamples -o TemporalExamples
	./TemporalExamples
