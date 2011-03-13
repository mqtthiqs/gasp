.PHONY: clean

abstract.pdf: abstract.tex

PDFLATEX=pdflatex
INCLUDE=include

%.pdf:%.tex
	TEXINPUTS=$(INCLUDE):$$TEXINPUTS $(PDFLATEX) $<
	TEXINPUTS=$(INCLUDE):$$TEXINPUTS $(PDFLATEX) $<

clean:
	rm -fr *.aux *.log *.out *.bbl abstract.pdf *.bbl *.blg *.brf 