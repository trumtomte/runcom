# Run commands

> Repository for run commands (aka. dotfiles) and more.

Test

## Colorscheme (Sherlock)

```
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

## Packages

```
git, vim (fzf.vim, vim-polygot), nnn, fzf, pass, aria2, ripgrep, bat, zsh
(zsh-syntax-highlighting), tmux (reattach-to-user-namespace), urlview, mu
(maildir-utils), isync (mbsync), w3m, lftp, imagemagick, ffmpeg, pandoc, elixir,
erlang, node, postgresql, etc...
```

## Locals

```
# .vimrc.local
set runtimepath+=/usr/bin/fzf
source /usr/share/doc/fzf/examples/fzf.vim
# .exports.local
export PATH=$PATH:$HOME/Git/elixir-ls
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/doc/fzf/examples/completion.zsh
source /usr/share/doc/fzf/examples/key-bindings.zsh
```
