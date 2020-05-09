#!/bin/bash

# Terminal
ln -s .aliases ~/.aliases
ln -s .exports ~/.exports
ln -s .functions ~/.functions
ln -s .zshrc ~/.zshrc
ln -s .tmux.conf ~/.tmux.conf

# REPL
ln -s .iex.exs ~/.iex.exs
ln -s .fxrc ~/.fxrc

# Mutt
mkdir ~/.mutt
ln -s .muttrc ~/.muttrc
ln -s .mbsyncrc ~/.mbsyncrc
ln -s .mailcap ~/.mutt/.mailcap
ln -s display_filter ~/.mutt/display_filter
ln -s view_attachment.sh ~/.mutt/view_attachment.sh

# Vim
mkdir ~/.vim/colors
ln -s .vimrc ~/.vimrc
ln -s sherlock.vim ~/.vim/colors/sherlock.vim

# Ranger
mkdir ~/.config/ranger/colorschems
ln -s rc.conf ~/.config/ranger/rc.conf
ln -s sherlock.py ~/.config/ranger/colorschemes/sherlock.py

# Misc
ln -s .lftprc ~/.lftprc

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
