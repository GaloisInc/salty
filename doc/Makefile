
all: salty.pdf

clean:
	$(RM) -r build
	$(RM) salty.pdf

build:
	mkdir build

salty.pdf: build/salty.pdf
	cp $< $@

build/%.tex: %.md | build
	pandoc --from markdown --to latex --standalone --toc --output $@ $<

%.pdf: %.tex
	cd $(dir $<) && pdflatex $(notdir $<)
