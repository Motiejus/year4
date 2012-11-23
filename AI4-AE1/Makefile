
.PHONY: all clean distclean

AGGR_PLOTS := res/aggr_e-m.tex res/aggr_e-z.tex res/aggr_m-z.tex
SAMPLE_PLOTS := res/sample_e.pdf res/sample_m.pdf res/sample_z.pdf

all: report.pdf

res/aggr_%.pdf: samples/summary.txt
	./plot.sh $*

res/sample_%.pdf: laboratory.dat
	./sensing.py $* $<

report.pdf: report.tex res/predictions.pdf res/prediction_table.tex \
	$(AGGR_PLOTS) $(SAMPLE_PLOTS)
	while (pdflatex report; grep -q "Rerun to" report.log) do true; done

res/predictions.pdf res/prediction_table.tex: report.py samples/summary.txt
	./report.py samples/summary.txt

samples:
	curl -L https://github.com/downloads/Motiejus/year4/AI4-AE1.tar.gz | tar -xz

samples/summary.txt: samples
	$(MAKE) $(MAKEFLAGS) -C samples

clean:
	rm -f *.pyc
	rm -fr res
	rm -f report.aux report.log report.pdf report.toc
	-$(MAKE) $(MAKEFLAGS) -C samples clean

distclean: clean
	rm -fr samples
