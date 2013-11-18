
tex  = $(wildcard *.tex)
pngs = $(tex:%.tex=%.png)

all: $(pngs)

%.png: %.tex
	pdflatex $<
	convert -density 300 $*.pdf $@

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.pdf
