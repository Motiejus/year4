.PHONY: all clean distclean

AGGR_PLOTS := res/aggr_e-m.pdf res/aggr_e-z.pdf res/aggr_m-z.pdf
SAMPLE_PLOTS := res/sample_e.pdf res/sample_m.pdf res/sample_z.pdf

SRCS := sensing.py analyze.py plot.sh report.py Makefile samples/Makefile

all: report.pdf

res:
	mkdir -p res

res/aggr_%.pdf: samples/summary.txt plot.sh res
	./plot.sh $*

res/sample_%.pdf: laboratory.dat sensing.py res
	./sensing.py $* $<

report.pdf: report.tex res/predictions.pdf res/report_data.tex \
	$(AGGR_PLOTS) $(SAMPLE_PLOTS) $(SRCS)
	while (pdflatex report; grep -q "Rerun to" report.log) do true; done

res/predictions.pdf res/report_data.tex: samples/summary.txt report.py res
	./report.py $<

samples/Makefile: samples

samples/summary.txt: samples
	$(MAKE) $(MAKEFLAGS) -C samples summary.txt

samples:
	curl -L https://github.com/downloads/Motiejus/year4/AI4-AE1.tar.gz | tar xz

clean:
	rm -f *.pyc
	rm -fr res
	rm -f report.aux report.log report.pdf report.toc report.out
	-$(MAKE) $(MAKEFLAGS) -C samples clean

distclean: clean
	rm -fr samples
