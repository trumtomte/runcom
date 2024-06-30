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

For example, `.zshrc.local`:

```
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/doc/fzf/examples/completion.zsh
source /usr/share/doc/fzf/examples/key-bindings.zsh
```

## MacOS

Dragable windows by `ctrl + cmd`:

```bash
$ defaults write -g NSWindowShouldDragOnGesture -bool yes
```

## Stuff to install

*General*

aria2, ffmpeg, fzf, git, imagemagick, lftp, nnn, pass, ripgrep, vim,
zsh, zsh-syntax-highlighting, emacs

qalculate, ghex/okteta, zathura, wireshark, zotero, vlc, discord,

papirus-icon-theme, IBM Plex font

## Getting started

mkdir: $HOME/.vim/{undo,colors}
ln: .{zshrc,vimrc,lftprc,emacs} to $HOME
ln: sherlock.vim to $HOME/.vim/colors
cp: terminalc to $HOME/.config/xfce4/terminal
