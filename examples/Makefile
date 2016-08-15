
SLUGS ?= ../../slugs/src/slugs

SALTY ?=

# Convenience for adding additional parameters to the salty command line
EXTRA ?=

ifeq ($(SALTY),)
  exists := $(shell test -f ../dist/build/salty/salty && echo "exists")
  ifeq ($(exists),exists)
    SALTY := ../dist/build/salty/salty
  else
    exists := $(shell test -f ../../cabal.project && echo "exists")
    ifeq ($(exists),exists)
      SALTY := $(wildcard ../../dist-newstyle/build/salty-*/build/salty/salty)
    else
      SALTY := stack exec -- salty
    endif
  endif
endif

salty = $(SALTY) -s $(SLUGS) $(EXTRA) --dot --python --java=$1 -o build $2

ifeq ($(V),)
  Q := @
else
  Q :=
endif

sources := $(wildcard *.salt)
jars    := $(addprefix build/,$(sources:.salt=.jar))
built   := $(addprefix build/,$(sources:.salt=/.token))

.PHONY: all build clean

all: $(jars)

build:
	mkdir build

clean:
	$(RM) -r build
	$(RM) DemoTest.class

svg: $(built)
	$(Q) cd build && \
	  find . -name \*.dot -exec dot -Tsvg -o{}.svg {} \;


DemoTest.class: build/demo.jar DemoTest.java
	$(Q) javac -classpath build/ DemoTest.java

run-%: %.class
	$(Q) java -classpath .:build/ $*

build/%.jar: build/%/.token
	$(Q) cd build && \
	  find $* -name \*.java -exec javac {} \; && \
	  find $* -name \*.class -exec jar cf $*.jar {} +

build/%/.token: %.salt | build
	$(Q) $(call salty,$*,$*.salt) && touch $@
