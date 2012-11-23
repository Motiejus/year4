
.PHONY: clean all

all: report.pdf

report.pdf: report.tex res/predictions.eps res/prediction_table.tex
	while (pdflatex report; grep -q "Rerun to" report.log) do true; done

res/predictions.eps res/prediction_table.tex: report.py samples/summary.txt
	./report.py samples/summary.txt

samples/summary.txt:
	$(MAKE) $(MAKEFLAGS) -C samples

clean:
	rm -f *.pyc
	rm -fr res
	rm -f report.aux report.log report.pdf
	$(MAKE) $(MAKEFLAGS) -C samples clean
