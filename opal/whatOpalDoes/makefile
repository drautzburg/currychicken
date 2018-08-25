DOC = doc
include ../../texmakefile

texfiles = whatOpalDoes opal items monitoring workorders productionBatch
TEXFILES = $(addprefix $(DOC)/,$(texfiles:=.tex))
.PRECIOUS: $(TEXFILES)

dotfiles = stateDependencies interpolating unpackComplete workorderStates
EPSFILES = $(addprefix $(DOC)/,$(dotfiles:=.eps))

pdffiles = opal
PDFFILES = $(addprefix $(DOC)/,$(pdffiles:=.pdf))


all: $(PDFFILES)

$(PDFFILES): $(EPSFILES) $(TEXFILES)




clean:
	rm -f $(tmpFiles) $(DOC)/$(tmpFiles)
