DOC = doc
include ../../texmakefile

texfiles = Products Products_bak1 Sop2 ProductDisambiguation
TEXFILES = $(addprefix $(DOC)/,$(texfiles:=.tex))
.PRECIOUS: $(TEXFILES)

dotfiles = ProductsSymbols ProductsUnpack ProductsPack ProductsSplit ProductsMerge ProductsRestrict
DOTFILES = $(addprefix $(DOC)/,$(dotfiles:=.eps))

pdffiles = Products Products_bak1 ProductDisambiguation
PDFFILES = $(addprefix $(DOC)/,$(pdffiles:=.pdf))


all: $(PDFFILES)

$(PDFFILES): $(DOTFILES) $(TEXFILES)




clean:
	rm -f $(tmpFiles) $(DOC)/$(tmpFiles)
