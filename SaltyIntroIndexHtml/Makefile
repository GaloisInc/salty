
all: salty.html

clean:
	$(RM) -r build
	$(RM) salty.html

%.html: %.md reveal.template
	pandoc --slide-level=2 --template=reveal.template --from=markdown --to=revealjs $< -o $@

%.pdf: build/%.pdf
	cp $< $@


build:
	mkdir build

build/%.tex: %.md | build
	pandoc --slide-level=2 --standalone --from=markdown --to=beamer $< -o $@

build/%.pdf: build/%.tex | build
	(cd build && pdflatex $*.tex && pdflatex $*.tex && pdflatex $*.tex)
