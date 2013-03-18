.PHONY: all clean

all: report.pdf

pdflatex=while (pdflatex report; grep -q "Rerun to" report.log) do true; done

report.toc: report.tex
	$(call pdflatex)

report.pdf: report.toc report.tex images/slowdown.pdf images/speedup.pdf
	$(call pdflatex)

images/slowdown.pdf: slowdown.gp 1-4-64_speedup.txt
	gnuplot < $<

images/speedup.pdf: speedup.gp 1-4-64_speedup.txt
	gnuplot < $<

clean:
	rm -f *.pyc
	rm -f report.aux report.log report.pdf report.toc report.out
