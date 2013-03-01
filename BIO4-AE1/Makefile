report.pdf: report.tex repeatmasker.txt
	while (pdflatex $<; grep -q "Rerun to" report.log ) do true; done
	
.PHONY: clean
clean:
	rm -f report.aux report.log report.out report.pdf report.toc report.txt
