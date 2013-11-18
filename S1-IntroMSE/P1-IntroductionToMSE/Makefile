#
DOC=IntroToMSE

all: $(DOC).md
	pandoc -t beamer -s --slide-level=1 $(DOC).md -o $(DOC).pdf -H tex/header.tex

notes: $(DOC).md
	pandoc -t beamer $(DOC).md -o $(DOC)Notes.pdf

show: $(DOC).pdf
	pdf-presenter-console -d 12 -l 2 $(DOC).pdf

dual: $(DOC)Notes.pdf
	dspdfviewer $(DOC).pdf

clean:
	rm -f $(DOC).aux $(DOC).log $(DOC).out

cleanall: clean
	rm -f $(DOC).pdf
