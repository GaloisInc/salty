
SLUGS ?= ../../slugs/src/slugs

salty = stack exec -- salty -s $(SLUGS) -p $1 -o build $2

build/foo: demo.salt | build
	$(call salty,foo,demo.salt)

build:
	mkdir build

clean:
	$(RM) -r build
