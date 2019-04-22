
SLUGS ?= slugs

SALTY ?=

# Convenience for adding additional parameters to the salty command line
EXTRA ?=

ifeq ($(SALTY),)
  exists := $(shell test -f ../dist/build/salty/salty && echo "exists")
  ifeq ($(exists),exists)
    SALTY := ../dist/build/salty/salty
    $(info INFO: Using salty from cabal dist directory)
  else
    exists := $(shell find ../dist-newstyle/build -name salty -type f)
    ifneq ($(exists),)
      SALTY := $(exists)
      $(info INFO: Using salty from cabal dist-newstyle directory)
    else
      SALTY := stack exec -- salty
      $(info INFO: Using stack to run salty)
    endif
  endif
endif

salty = $(SALTY) -s $(SLUGS) $(EXTRA) --dot --python --spark --java=$1 --cpp -o build $2

ifeq ($(V),)
  Q := @
else
  Q :=
endif

sources := $(wildcard *.salt)
jars    := $(addprefix build/,$(sources:.salt=.jar))
built   := $(addprefix build/,$(sources:.salt=/.token))

test_sources := $(patsubst %.salt.test,%.salt,$(wildcard *.salt.test))

.PHONY: all gen clean

all: $(jars)

build:
	mkdir build

clean:
	$(RM) -r build
	$(RM) DemoTest.class

gen: $(built)

gen-%.salt: %.salt
	$(Q) $(MAKE) build/$*/.token

gen-test: $(addprefix gen-,$(test_sources))

build-objs: gen-test | build
	$(Q) (cd build; g++ -c -std=c++11 *.cpp)

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

echo-%:
	$(Q) echo "$* = $($*)"
