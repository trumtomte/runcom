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

  gray0   = 234 #1c1c1c 0xea
  gray1   = 235 #262626 0xeb
  gray2   = 243 #767676 0xf3
  gray3   = 236 #303030 0xec
  gray4   = 240

Local configuration for ``zsh`` and ``vim`` are loaded from ``$HOME/.zshrc.local`` and ``$HOME/.vimrc.local`` respectively.

For example, ``.zshrc.local`` with zsh syntax and fzf::

  source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
  source /usr/share/doc/fzf/examples/completion.zsh
  source /usr/share/doc/fzf/examples/key-bindings.zsh

=====
MacOS
=====

Dragable windows by ``ctrl + cmd``::

  $ defaults write -g NSWindowShouldDragOnGesture -bool yes

=======
Install
=======

Braindump of stuff that I might install.

aria2, ffmpeg, fzf, git, imagemagick, lftp, nnn, pass, ripgrep, vim,
zsh, zsh-syntax-highlighting, emacs, qalculate, ghex/okteta, zathura,
wireshark, zotero, vlc, discord, papirus-icon-theme, IBM Plex font

=====
Setup
=====

* mkdir $HOME/.vim/{undo,colors}
* ln -s .{zshrc,vimrc,lftprc,emacs} $HOME
* ln -s sherlock.vim $HOME/.vim/colors
* cp terminalc $HOME/.config/xfce4/terminal
