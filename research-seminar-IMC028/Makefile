PDFLATEXFLAGS=-halt-on-error

klinik2012ResearchSeminar.pdf: klinik2012ResearchSeminar.tex computer_science.bib
	pdflatex ${PDFLATEXFLAGS} klinik2012ResearchSeminar

klinik2012ResearchSeminarPresentation.pdf: klinik2012ResearchSeminarPresentation.tex
	pdflatex ${PDFLATEXFLAGS} klinik2012ResearchSeminarPresentation

klinik2013ResearchSeminarPresentation2.pdf: klinik2013ResearchSeminarPresentation2.tex
	pdflatex ${PDFLATEXFLAGS} klinik2013ResearchSeminarPresentation2

remake:
	pdflatex ${PDFLATEXFLAGS} klinik2012ResearchSeminar
	bibtex klinik2012ResearchSeminar
	pdflatex ${PDFLATEXFLAGS} klinik2012ResearchSeminar
	pdflatex ${PDFLATEXFLAGS} klinik2012ResearchSeminar
