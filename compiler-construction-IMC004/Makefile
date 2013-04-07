PDFLATEXFLAGS=-halt-on-error

klinik2013CompilerConstructionTypechecker.pdf: klinik2013CompilerConstructionTypechecker.tex
	pdflatex ${PDFLATEXFLAGS} $^

remake:
	pdflatex ${PDFLATEXFLAGS} klinik2013CompilerConstructionTypechecker
	bibtex klinik2013CompilerConstructionTypechecker
	pdflatex ${PDFLATEXFLAGS} klinik2013CompilerConstructionTypechecker
	pdflatex ${PDFLATEXFLAGS} klinik2013CompilerConstructionTypechecker
