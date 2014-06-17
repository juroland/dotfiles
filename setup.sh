#!/bin/bash

files=".zshrc .vimrc .gitconfig"
directories=".atom"
scriptname=$(readlink -f $0)
dotfilesdir=$(dirname $scriptname)

for file in $files; do
  ln -si $dotfilesdir/$file ~/$file
done

for directory in $directories; do
  for file in $(ls $dotfilesdir/$directory); do
    ln -si $dotfilesdir/$directory/$file ~/$directory/$file
  done
done
