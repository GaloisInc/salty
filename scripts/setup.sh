#!/bin/bash

set -ev

# Setup ~/.local/bin
if [ ! -d ~/.local/bin ]; then
    mkdir -p ~/.local/bin
fi

if [ ! -f ~/.local/bin/stack ]; then
    curl -L https://www.stackage.org/stack/linux-x86_64 \
      | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
fi

if [ ! -h ~/.local/bin/g++ ]; then
    ln -s /usr/bin/g++-4.9 ~/.local/bin/g++
fi

export PATH=$HOME/.local/bin:$PATH

if [ ! -d ~/slugs-master/src ]; then
    curl -LO https://github.com/VerifiableRobotics/slugs/archive/master.zip
    unzip master.zip -d ~
    rm master.zip
fi

if [ ! -f ~/slugs-master/src/slugs ]; then
    (cd ~/slugs-master/src && make)
fi

if [ ! -f ~/.local/bin/z3 ]; then
    curl http://saw.galois.com/builds/z3/z3 > ~/.local/bin/z3
    chmod +x ~/.local/bin/z3
fi
