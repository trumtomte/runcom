# Run commands

_Repository for run commands (aka. dotfiles) and more._

My own custom colorscheme "Sherlock".

```
white   = 7   #fbf8ff 0x07
beige   = 223 #ffd7af 0xdf
yellow  = 179 #d7af5f 0xb3
red     = 167 #d75f5f 0xa7
blue    = 109 #87afaf 0x6d
green   = 71  #5faf5f 0x47
magenta =     #C374B4
cyan    =     #45B8B1

gray0   = 234 #1c1c1c 0xea
gray1   = 235 #262626 0xeb
gray2   = 243 #767676 0xf3
gray3   = 236 #303030 0xec
gray4   = 240
```

Local configuration for `zsh` and `vim` are loaded from `$HOME/.zshrc.local` and
`$HOME/.vimrc.local` respectively.

## TODO

- [ ] Make setup.sh prompt for what files are to be symlinked
- [ ] Setup for BASH

## MacOS

Dragable windows by `ctrl + cmd`:

```bash
$ defaults write -g NSWindowShouldDragOnGesture -bool yes
```

## Packages

aria2 bat ffmpeg fzf git imagemagick isync lftp maildir-utils mu nnn pandoc pass
ripgrep vim (fzf.vim, vim-polygot) zsh zsh-syntax-highlighting

## Getting started

mkdirs: tmp, Torrents, .vim/{undo,colors,pack}
symlinks: .{aliases,exports,zshrc,vimrc,mbsyncrc,lftprc} to home
symlinks: sherlock.vim to .vim/colors
cp doom config.el to doom dir
