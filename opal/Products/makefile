DOC = doc
include ../../texmakefile

texfiles = Products Products_bak1 Sop2
TEXFILES = $(addprefix $(DOC)/,$(texfiles:=.tex))
.PRECIOUS: $(TEXFILES)

dotfiles = ProductsSymbols ProductsUnpack ProductsPack ProductsSplit ProductsMerge ProductsRestrict
DOTFILES = $(addprefix $(DOC)/,$(dotfiles:=.eps))

pdffiles = Products Products_bak1
PDFFILES = $(addprefix $(DOC)/,$(pdffiles:=.pdf))


all: $(PDFFILES)

$(DOC)/Products.pdf: $(DOTFILES) $(TEXFILES)
$(DOC)/Products_bak1.pdf: $(DOTFILES) $(TEXFILES)



clean:
	rm -f $(tmpFiles) $(DOC)/$(tmpFiles)
