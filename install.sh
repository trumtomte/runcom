#!/bin/bash

# NOTE: has to be run from this directory

# Directories
# ===========
mkdir ~/{tmp,Torrents}
mkdir -p ~/.mutt/{undo,tmp/attachments}
mkdir -p ~/.vim/{colors,bundle}

# Symlinks
# ========
ln -s .{aliases,exports,functions,zshrc,vimrc,tmux.conf,muttrc,mbsyncrc} ~/
ln -s {.mailcap,sherlock.mutt,open_mutt_attachment.sh} ~/.mutt/
ln -s sherlock.vim ~/.vim/colors/sherlock.vim
ln -s .{lftprc,iex.exs} ~/

# MacOS
# =====
# Disable transparency in the menu bar and elsewhere on Yosemite
defaults write com.apple.universalaccess reduceTransparency -bool true
# Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
defaults write com.apple.screencapture type -string "png"
# Finder: show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
# Set Safari’s home page to `about:blank` for faster loading
defaults write com.apple.Safari HomePage -string "about:blank"
