PDFLATEXFLAGS=-halt-on-error

klinik2013klinik2013TypecheckerPresentation.pdf: klinik2013TypecheckerPresentation.tex
	pdflatex ${PDFLATEXFLAGS} $^

klinik2013CompilerConstruction.pdf: klinik2013CompilerConstruction.tex
	pdflatex ${PDFLATEXFLAGS} $^

remake:
	pdflatex ${PDFLATEXFLAGS} klinik2013CompilerConstruction
	bibtex klinik2013CompilerConstruction
	pdflatex ${PDFLATEXFLAGS} klinik2013CompilerConstruction
	pdflatex ${PDFLATEXFLAGS} klinik2013CompilerConstruction
