============
Run commands
============

Repository for run commands (aka. dotfiles) and more.

Custom colorscheme named "Sherlock"::

  main-bg    "#262626"
  main-fg    "#ffd7af"
  dark-bg    "#1f1f1f"
  light-bg   "#2c2c2c"
  white      "#fbf8ff"
  red        "#e06767"
  red-bg     "#432f2f"
  green      "#5faf5f"
  green-bg   "#2c352c"
  blue       "#87afaf"
  blue-bg    "#313636"
  yellow     "#d7af5f"
  yellow-bg  "#38342c"
  violet     "#cdb5cd"
  cyan       "#87af97"
  gray-1     "#303030"
  gray-2     "#404040"
  gray-3     "#585858"
  gray-4     "#767676"
  
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
