# Run commands

Repository for run commands (ie. dotfiles) and more.

## Small tidbits

Create a timelapse of screenshots.

```bash
# Saves a screenshot every 1.5s

while true; do
    scrot
    sleep 1.5s
end;

# Use ffmpeg to compose a timelapse.
```

Mac OS defaults:

```bash
# Disable transparency in the menu bar and elsewhere on Yosemite
defaults write com.apple.universalaccess reduceTransparency -bool true

# Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
defaults write com.apple.screencapture type -string "png"

# Finder: show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Set Safari’s home page to `about:blank` for faster loading
defaults write com.apple.Safari HomePage -string "about:blank"
```
