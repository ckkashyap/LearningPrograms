sources=$(wildcard *.txt)
pdfs=$(patsubst %.txt, %.pdf, $(sources))

all: $(pdfs)

%.pdf:%.tex
	pdflatex $<
	pdflatex $<
	x=`basename $< .tex`; mv $$x.txt __$$x.txt; \
	mv $$x.pdf __$$x.pdf; rm -f $$x*; mv __$$x.txt $$x.txt ; \
	mv __$$x.pdf $$x.pdf;
	echo "$@" >> junk_list



%.html:%.txt
	perl Markdown.pl $< > $@
	echo "$@" >> junk_list

%.tex:%.html
	perl mdbeamer.pl $< > $@
	echo "$@" >> junk_list


clean:
	rm -f $$(cat junk_list)
	> junk_list
