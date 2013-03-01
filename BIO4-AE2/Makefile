.PHONY: all clean

all: report.pdf

pdflatex=while (pdflatex report; grep -q "Rerun to" report.log) do true; done

report.toc: report.tex
	$(call pdflatex)

report.pdf: report.toc report.tex 
	$(call pdflatex)

clean:
	rm -f *.pyc
	rm -f report.aux report.log report.pdf report.toc report.out
