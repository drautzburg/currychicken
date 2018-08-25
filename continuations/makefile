DOC = doc
include ../texmakefile

texfiles = concurrencyContinuations.tex  MonadsInSlowMo.tex
TEXFILES = $(addprefix $(DOC)/,$(texfiles))


all: $(TEXFILES:.tex=.pdf)

clean:
	rm -f $(tmpFiles) $(DOC)/$(tmpFiles)
