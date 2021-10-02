# Run commands

> Repository for run commands (aka. dotfiles) and more.

## TODO

- [ ] Proper setup script

## Packages

- git
- vim
- nnn
- fzf
- pass
- aria2
- ripgrep
- bat
- zsh, zsh-syntax-highlighting
- tmux, reattach-to-user-namespace
- mutt, urlview, mu, isync (mbsync), w3m
- asdf
- tig
- lftp
- imagemagick
- pandoc, basictex
- elixir, erlang, ruby, node (via asdf)
- postgresql

*Misc: libxml2, coreutils, automake, autoconf, openssl, libyaml, readline,
libxslt, libtool, watchman*

## Terminal colorscheme: Sherlock

```txt
white   = 7    #fbf8ff
beige   = 223  #ffd7af 0xdf
yellow  = 179  #d7af5f 0xb3
red     = 167  #d75f5f 0xa7
blue    = 109  #87afaf 0x6d
green   = 71   #5faf5f
gray1   = 235  #262626
gray2   = 243  #767676 0xf3

gray_misc1 = 234 #1c1c1c 0xea
gray_misc2 = 235 #262626 0xeb
gray_misc3 = 236 #303030 0xec
gray_misc4 = 240
```

## Mutt account template

```muttrc
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

## Reminder: Apps

- Browsers: Qutebrowser, Firefox
- Chat: Discord, Slack, Teams, Zoom
- Music: Spotify, (Ozone 8, Youlean Loudness Meter 2)
- Work: Docker, VirtualBox
- Misc: VLC, Mullvad VPN, Rectangle (WM), Amphetamine
