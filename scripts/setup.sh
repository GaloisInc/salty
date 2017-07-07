#!/bin/bash

set -ev

# Setup ~/.local/bin
if [ ! -d ~/.local/bin ]; then
    mkdir -p ~/.local/bin
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

if [ ! -f ~/.local/bin/slugs ]; then
    (cd ~/slugs-master/src && make && cp slugs ~/.local/bin)
fi

if [ ! -f ~/.local/bin/z3 ]; then
    curl https://saw.galois.com/builds/z3/z3 > ~/.local/bin/z3
    chmod +x ~/.local/bin/z3
fi

if [ ! -d language-slugs ]; then
    git clone https://github.com/galoisinc/language-slugs
else
    (cd language-slugs && git pull)
fi
