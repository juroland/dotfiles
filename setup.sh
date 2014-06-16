#!/bin/bash

files="zshrc"
scriptname=$(readlink -f $0)
dotfilesdir=$(dirname $scriptname)

echo $dotfilesdir

for file in $files; do
  ln -si $dotfilesdir/$file ~/.$file
done
