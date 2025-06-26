SLIDES=slides-pltc
SLIDESPDF=$(SLIDES).pdf

PDFLATEX=pdflatex -halt-on-error

.PHONY: all
all: $(SLIDESPDF)

.PHONY: clean
clean:
	rm -rf *~ *.aux *.log $(SLIDESPDF) *.bbl *.blg *.toc *.out *.vrb *.snb *.nav *.snm

.PHONY: help
help:
	@echo "Possible targets:"
	@echo "  make clean   : cleanup directory"
	@echo "  make all     : build slides"

$(SLIDESPDF): $(SLIDES).tex Makefile
	$(PDFLATEX) $<
	$(PDFLATEX) $<
