#!/bin/bash

files=".tmux.conf .zshrc .vimrc .gitconfig .emacs .idevimrc"
directories=".atom"
scriptname=$(readlink -f $0)
dotfilesdir=$(dirname $scriptname)
vundle_dir=~/.vim/bundle/Vundle.vim

function setup_fonts {
  git clone https://github.com/powerline/fonts /tmp/fonts
  mkdir -p ~/.fonts
  mv /tmp/fonts/SourceCodePro/*.otf ~/.fonts
  fc-cache -vf ~/.fonts/
}

for file in $files; do
  ln -si $dotfilesdir/$file ~/$file
done

for directory in $directories; do
  for file in $(ls $dotfilesdir/$directory); do
    ln -si $dotfilesdir/$directory/$file ~/$directory/$file
  done
done

if test ! -e $vundle_dir; then
  git clone https://github.com/VundleVim/Vundle.vim.git $vundle_dir
fi

setup_fonts
