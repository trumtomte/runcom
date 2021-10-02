#!/bin/bash

# NOTE: has to be run from this directory

# MacOS
# =====
# Disable transparency in the menu bar and elsewhere
defaults write com.apple.universalaccess reduceTransparency -bool true
# Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
defaults write com.apple.screencapture type -string "png"
# Finder: show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
# Set Safari’s home page to `about:blank` for faster loading
defaults write com.apple.Safari HomePage -string "about:blank"
# Hide icons on the desktop (might require a Finder restart)
defaults write com.apple.finder CreateDesktop false
