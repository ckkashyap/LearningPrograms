#if DEBUG
#endif

texfiles=$(wildcard *.tex)
basenames=$(patsubst %.tex, %, $(texfiles))
dvifiles=$(patsubst %.tex, %.dvi, $(texfiles))

garbage=.dvi,.log,.aux,.ps,.pdf,.out,.nav,.toc,.snm,.vrb


%.dvi:%.tex
	latex $<

%.pdf:%.tex
	latex $<
	dvipdf $(patsubst %.tex,%.dvi,$<)

all:
	@for i in $(texfiles); do echo Building $$i; latex $$i 2>&1 > /dev/null; done 
	@for i in $(dvifiles); do echo Building $$i; dvipdf $$i 2>&1 > /dev/null; done 

clean:
	@for i in $(basenames); do rm -f $$i{$(garbage)}; done
