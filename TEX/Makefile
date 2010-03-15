#if DEBUG
#endif

texfiles=$(wildcard *.tex)
basenames=$(patsubst %.tex, %, $(texfiles))


%.dvi:%.tex
	latex $<


all:
	@for i in $(texfiles); do echo Building $$i; latex $$i 2>&1 > /dev/null; done 

clean:
	@for i in $(basenames); do echo Removing $$i{.dvi,.log,.aux,.ps}; rm -f  $$i{.dvi,.log,.aux,.ps}; done
