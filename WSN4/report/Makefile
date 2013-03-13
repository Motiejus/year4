.PHONY: all clean

IMGS = test_suite.png TinyBlogAppC.png ui_run.png

all: report.pdf

pdflatex=while (pdflatex report; grep -q "Rerun to" report.log) do true; done

report.toc: report.tex
	$(call pdflatex)

report.pdf: report.toc report.tex $(wildcard *.png)
	$(call pdflatex)

clean:
	rm -f report.aux report.log report.pdf report.toc report.out
