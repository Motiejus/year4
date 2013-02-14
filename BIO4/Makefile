report.pdf: report.tex repeatmasker.txt
	while (pdflatex $<; grep -q "Rerun to" report.log ) do true; done
