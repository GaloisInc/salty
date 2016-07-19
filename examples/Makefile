
SLUGS ?= ../../slugs/src/slugs

salty = stack exec -- salty -s $(SLUGS) -p $1 -o build $2

all: DemoTest.class

run-%: %.class
	java -classpath .:build/ $*


build:
	mkdir build

define build-jar

build/$1.jar: build/$1/.token
	cd build && \
	  find demo -name \*.java -exec javac {} \; && \
	  find demo -name \*.class -exec jar cf $1.jar {} +

build/$1/.token: $1.salt | build
	$$(call salty,$1,$1.salt) && touch $$@

endef

clean:
	$(RM) -r build
	$(RM) DemoTest.class

$(eval $(call build-jar,demo))

DemoTest.class: build/demo.jar DemoTest.java
	javac -classpath build/ DemoTest.java
