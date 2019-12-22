DOC = doc
include ../texmakefile



pdffiles = concurrencyContinuations  MonadsInSlowMo Cont18-Main
PDFFILES = $(addprefix $(DOC)/,$(pdffiles:=.pdf))

allPdf: $(PDFFILES)

#$(DOC)/concurrencyContinuations.pdf : $(DOC)/concurrencyContinuations.tex
#$(DOC)/MonadsInSlowMo.pdf           : $(DOC)/MonadsInSlowMo.tex

Cont18files = Cont18-Main Cont18-Cont Cont18-Threads
Cont18Texfiles = $(addprefix $(DOC)/, $(Cont18files:=.tex))
$(DOC)/Cont18-Main.pdf               : $(Cont18Texfiles) 


.DELETE_ON_ERROR:
$(V).SILENT:

clean:
	rm -f $(tmpFiles) $(DOC)/$(tmpFiles)
