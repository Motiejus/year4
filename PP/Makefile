.PHONY: all clean

all: report.pdf presentation.pdf

pdflatex=while (pdflatex $1; grep -q \"$2\" $1.log) do true; done

report.toc: report.tex
	$(call pdflatex,report,"Rerun to")

report.pdf: report.toc report.tex images/slowdown.pdf images/speedup.pdf
	$(call pdflatex,report,"Rerun to")

presentation.pdf: presentation.tex images/slowdown.pdf images/speedup.pdf
	$(call pdflatex,presentation,"No file")

images/slowdown.pdf: slowdown.gp 1-4-64_speedup.txt
	gnuplot < $<

images/speedup.pdf: speedup.gp 1-4-64_speedup.txt
	gnuplot < $<

clean:
	rm -f *.pyc
	rm -f report.aux report.log report.pdf report.toc report.out
	rm -f images/speedup.pdf images/slowdown.pdf
	rm -f presentation.aux presentation.log presentation.pdf \
		presentation.toc presentation.out presentation.snm presentation.nav
