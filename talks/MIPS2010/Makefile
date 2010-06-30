.PHONY: all clean examples

all: slides.tex
	TEXINPUTS=common:${TEXINPUTS} pdflatex slides.tex
	TEXINPUTS=common:${TEXINPUTS} pdflatex slides.tex
	TEXINPUTS=common:${TEXINPUTS} pdflatex slides.tex

slides.tex: main.tex
	../../tools/latexpp/latexpp.opt -g color yes main.tex > slides.tex


clean:
	rm --force *.log *.aux *.vrb *.toc *~ *.nav *.out *.snm *.pdf slides.tex

