.PHONY: all clean

all: report.pdf presentation.pdf

pdflatex=while (pdflatex $1; grep -q \"$2\" $1.log) do true; done

GENIMGS = images/speedup.pdf images/slowdown.pdf

PIMGS = $(wildcard images/*.png)

report.toc: report.tex $(GENIMGS)
	$(call pdflatex,report,"Rerun to")

report.pdf: report.toc report.tex $(GENIMGS)
	$(call pdflatex,report,"Rerun to")

presentation.toc: presentation.tex $(GENIMGS)
	$(call pdflatex,presentation,"No file")

presentation.pdf: presentation.toc presentation.tex $(GENIMGS)
	$(call pdflatex,presentation,"No file")

images/slowdown.pdf: slowdown.gp 1-4-56_speedup.txt
	gnuplot < $<

images/speedup.pdf: speedup.gp 1-4-56_speedup.txt
	gnuplot < $<

clean:
	rm -f *.pyc
	rm -f report.aux report.log report.pdf report.toc report.out
	rm -f images/speedup.pdf images/slowdown.pdf
	rm -f presentation.aux presentation.log presentation.pdf presentation.vrb \
		presentation.toc presentation.out presentation.snm presentation.nav
