
SLUGS ?= ../../slugs/src/slugs

SALTY ?=

ifeq ($(SALTY),)
  exists := $(shell test -f ../dist/build/salty/salty && echo "exists")
  ifeq ($(exists),exists)
    SALTY := ../dist/build/salty/salty
  else
    SALTY := stack exec -- salty
  endif
endif

salty = $(SALTY) -s $(SLUGS) --python --java=$1 -o build $2

ifeq ($(V),)
  Q := @
else
  Q :=
endif

sources := $(wildcard *.salt)
jars    := $(addprefix build/,$(sources:.salt=.jar))

all: $(jars)

build:
	mkdir build

clean:
	$(RM) -r build
	$(RM) DemoTest.class

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
