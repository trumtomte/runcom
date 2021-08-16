# Run commands

## TODO

Installation script for servers

Repository for run commands (ie. dotfiles) and more.

## CLI tools

- [asdf](https://asdf-vm.com/#/) to manange versions of CLI tools (node, etc.)
- [ripgrep](https://github.com/BurntSushi/ripgrep/), substitute for `grep`
- [bat](https://github.com/sharkdp/bat), substitute for `cat`
- tmux - screen multiplexer
    - reattach-to-user-namespace (copy/paste for MacOS)
- mutt - email
    - urlview - extract urls in mutt
    - mu - email indexing
    - isync (mbsync)
    - w3m (render HTML emails)
- nnn - directory navigator
- zsh - shell
    - zsh-syntax-highlighting
- aria2 - torrents
- pass - password manager
- git
- tig - git log viewer
- vim - editor
- postgresql
- imagemagick
- lftp
- fzf (fuzzy finder)
- elixir/erlang
- ruby

*Misc: libxml2, coreutils, automake, autoconf, openssl, libyaml, readline,
libxslt, libtool, watchman, java, pandoc, basictex*

*Misc 2: jq (json query), jo (create json), fx (navigate json)*

Run install script for fzf
add lftp bookmarks to this repository?

## Apps

- Browsers: Qutebrowser, Firefox, Chrome
- Chat: Discord, Slack, Teams, Zoom
- Music: Spotify, (Ozone 8, Youlean Loudness Meter 2)
- Misc: VLC, Mullvad VPN, Docker, Rectangle (VM), VirtualBox

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

## Colorscheme: Sherlock

```
white = 7 rgb(250, 247, 255), should be 15 (xterm)?
beige = 223 (#ffd7af) 0xdf
yellow = 179 (#d7af5f) 0xb3
red = 167 (#d75f5f) 0xa7
blue = 109 (#87afaf) 0x6d
gray_light = 243 (#767676) 0xf3
gray_variants = 234/235/236 (#1c1c1c/#262626/#303030) 0xea/0xeb/0xec
```

## Mutt

```
# vim: ft=muttrc
set from = "Sebastian Bengtegård <sebastian@mail.com>"
set realname = "Sebastian Bengtegård"
set smtp_url = "smtp://sebastian@mail.com@hostname.com:587/"
set smtp_pass = $my_something_pass
set spoolfile = "+something/INBOX"
set mbox = "+something/archive"
set record = "+something/sent"
set trash = "+something/deleted"

macro index,pager a \
    "<save-message>+something/archive<enter>"  \
    "move message to the archive"

macro index,pager i \
    "<save-message>+something/INBOX<enter>"  \
    "move message to the inbox"
```
