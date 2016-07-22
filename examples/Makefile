
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

salty = $(SALTY) -s $(SLUGS) -p $1 -o build $2

ifeq ($(V),)
  Q := @
else
  Q :=
endif

all: DemoTest.class

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
