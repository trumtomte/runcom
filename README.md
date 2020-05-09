# Run commands

Repository for run commands (ie. dotfiles) and more.

## CLI tools

* [asdf](https://asdf-vm.com/#/) to manange versions of CLI tools (node, etc.)
* [ripgrep](https://github.com/BurntSushi/ripgrep/), substitute for `grep`
* [bat](https://github.com/sharkdp/bat), substitute for `cat`
* tmux - screen multiplexer
* mutt - email
* mu - email indexing
* ranger - directory navigator
* zsh - shell
* aria2 - torrents
* pass - password manager
* git
* tig - git log viewer
* vim - editor
* zsh-syntax-highlight
* postgresql
* imagemagick
* elixir/erlang
* ruby
* reattach-to-user-namespace - copy/paste tmux/macos
* urlview - extract urls in mutt
* lftp - ftp
* jq - json query
* jo - make json
* fx - navigate json

*Misc: libxml2, coreutils, automake, autoconf, openssl, libyaml, readline,
libxslt, libtool, watchman, java*

## Apps

* FireFox and Chrome
* Slack and Discord
* Docker
* Mullvad VPN
* VLC
* Spotify, Ozone 8 and Youlean Loudness Meter 2
* Spectacle and Amphetamine

*Work: Zoom, Microsoft Teams, Skype, VirtualBox*

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
