#!/bin/bash

# NOTE: has to be run from this directory

# Directories
# ===========
mkdir ~/{tmp,Torrents}
mkdir -p ~/.mutt/{undo,tmp/attachments}
mkdir -p ~/.vim/{colors,bundle}

# Symlinks
# ========
ln -s .{aliases,exports,zshrc,vimrc,tmux.conf,muttrc,mbsyncrc} ~/
ln -s {.mailcap,sherlock.mutt,open_mutt_attachment.sh} ~/.mutt/
ln -s sherlock.vim ~/.vim/colors/sherlock.vim
ln -s .{lftprc,iex.exs} ~/
