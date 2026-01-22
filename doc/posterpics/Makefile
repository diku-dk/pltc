PAPER=posterpics

.PHONY: all
all: $(PAPER).pdf

$(PAPER).pdf: $(PAPER).tex Makefile
	pdflatex $(PAPER).tex

.PHONY: clean
clean:
	rm -rf *~ *.log *.auc $(PAPER).pdf *.nav *.snm *.toc *.aux *.bbl *.out *.blg

# Check the dimensions of all embedded pictures - we want to make sure that the pictures are
# square. Notice that we assume all pictures to be stored as .jpg files.
.PHONY: check
check:
	magick identify -format "%f: %wx%h\n" ../pics/*.jpg
