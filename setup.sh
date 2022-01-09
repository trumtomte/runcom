#!/bin/bash

mkdir -p $HOME/{tmp,Torrents,.vim,.doom.d}
mkdir -p $HOME/.vim/{undo,colors,pack}

ln -s $PWD/.{aliases,exports,zshrc,vimrc,tmux.conf,mbsyncrc,lftprc} $HOME/
ln -s $PWD/sherlock.vim $HOME/.vim/colors/
ln -s $PWD/.doom.d/{config.el,init.el,packages.el} $HOME/.doom.d/
