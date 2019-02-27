#!/bin/sh

set -e

# remove installed by default 
rm -f ~/.bashrc \
   ~/.login \
   ~/.profile

src=$PWD
for f in .????*; do
    rm -f ~/$f
    (cd ~/; ln -s $src/$f $f)
done

if [ ! -d ~/bin/common ]; then
    mkdir -p ~/bin
    (cd ~/bin; ln -s $src/bin/common common)
fi


