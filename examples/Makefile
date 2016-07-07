
SLUGS ?= ../../slugs/src/slugs

salty = stack exec -- salty -s $(SLUGS) -p $1 -o build $2

all: build/demo

build/%: %.salt | build
	$(call salty,$*,demo.salt)

build:
	mkdir build

clean:
	$(RM) -r build
