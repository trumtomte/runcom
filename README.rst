============
Run commands
============

Repository for run commands (aka. dotfiles) and more.

Custom colorscheme named "Sherlock"::

  white   = 7   #fbf8ff 0x07
  beige   = 223 #ffd7af 0xdf
  yellow  = 179 #d7af5f 0xb3
  red     = 167 #d75f5f 0xa7
  blue    = 109 #87afaf 0x6d
  green   = 71  #5faf5f 0x47
  magenta =     #C374B4
  cyan    =     #45B8B1
  gray1   = 234 #1c1c1c 0xea
  gray2   = 235 #262626 0xeb
  gray3   = 236 #303030 0xec
  gray4   = 240 #585858
  gray5   = 243 #767676 0xf3

Local configuration for ``zsh`` and ``vim`` are loaded from ``$HOME/.zshrc.local`` and ``$HOME/.vimrc.local`` respectively.

For example, ``.zshrc.local`` with zsh syntax and fzf::

  source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
  source /usr/share/doc/fzf/examples/completion.zsh
  source /usr/share/doc/fzf/examples/key-bindings.zsh

========
Packages
========

Braindump of stuff that I might install.

aria2, ffmpeg, fzf, git, imagemagick, lftp, nnn, pass, ripgrep, vim,
shellcheck, zsh, zsh-syntax-highlighting, emacs, wireshark, zotero,
vlc, discord, IBM Plex font, imhex

=====
Setup
=====

* mkdir $HOME/.vim/{undo,colors}
* ln -s .{zshrc,vimrc,lftprc,emacs} $HOME
* ln -s sherlock.vim $HOME/.vim/colors
* cp terminalc $HOME/.config/xfce4/terminal
