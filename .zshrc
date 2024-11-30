export HISTFILE="${ZDOTDIR:-$HOME}/.zsh_history"
export HISTSIZE=100000
export SAVEHIST=100000
export HISTORY_IGNORE="(ls|ls *|ll|ll *|cd|cd *|pwd|exit|date|* --help|* -h|n|nnn|vim *|man *|history|history *)"

setopt prompt_subst         # prompts for parameter expansions, etc.
setopt no_match             # errors for non matching filenames
setopt inc_append_history   # continously add to history
setopt extended_history     # add timestamp and elapsed time
setopt hist_ignore_dups     # ignore duplicate previous commands
setopt no_case_glob         # case insensitive globbing
setopt no_beep              # silence!

# Completion system, colornames, editable command line
zmodload zsh/complist
autoload -Uz compinit colors edit-command-line
compinit && colors
zle -N edit-command-line

PROMPT="%{$fg[blue]%}%n%{$fg[yellow]%}:%{$fg[blue]%}%m %{$fg[yellow]%}%3~ %{$fg[yellow]%}λ%{$reset_color%} "
# Show nnn's current depth (level)
[ -n "$NNNLVL" ] && PROMPT="nnn($NNNLVL) $PROMPT"

# menu selection, case insensitive match, list all files
zstyle ':completion:*'  menu select
zstyle ':completion:*'  matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*'  file-list all

bindkey '^[[1;5D' vi-backward-blank-word
bindkey '^[[1;5C' vi-forward-blank-word
bindkey '^X^E' edit-command-line

alias ll="ls -ahlF --color=auto"
alias n=nnn
alias grep="grep --color"
alias rgrep="rgrep --color"
alias gs="git status"
alias gap="git add -p"
alias gc="git commit"
alias gd="git diff"

br() {
  xrandr --ouput eDP-1 --brighness "$1"
}

dl() {
  aria2c "$1" --dir="$HOME/Downloads"
}

mov2mp4() {
  ffmpeg -i "$1" -q:v 0 "${1%.*}.mp4"
}

wav2mp3() {
  ffmpeg -i "$1" -codec:a libmp3lame -qscale:a 2 "${1%.*}.mp3"
}

md2pdf() {
  pandoc "$1" -o "${1%.*}.pdf" \
	 --number-sections \
	 -V geometry:margin=1.2in \
	 -V fontsize=11pt \
	 -V linestretch=1.3
}

# Un{zip,rar} stuff
unpack() {
  for i in *.zip; do
    [ -f "$i" ] || break
    zipfile="$i"
    zipdir="${i%.zip}"

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
      unzip "$zipfile" -d "$zipdir"
    elif [[ "$OSTYPE" == "darwin"* ]]; then
      ditto -V -x -k --sequesterRsrc --rsrc "$zipfile" "$zipdir"
    fi
  done

  for i in *.rar; do
    [ -f "$i" ] || break
    rarfile="$i"
    rardir="${i%.rar}"
    unrar x "$rarfile" "$rardir/"
  done
}

export VISUAL="vim"
export EDITOR="vim"
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export ASAN_OPTIONS=abort_on_error=1:halt_on_error=1
export UBSAN_OPTIONS=abort_on_error=1:halt_on_error=1

export NNN_OPTS="d"
export NNN_FIFO=$HOME/nnn.fifo
export NNN_FCOLORS="0000b3070000f3f3006d6da7"
export NNN_COLORS="#f36da7b3"
export NNN_OPENER="vim"

export FZF_DEFAULT_OPTS="
--height 80% --layout=reverse
--color fg:243,bg:235,hl:223,fg+:179,bg+:234,hl+:179
--color gutter:235,separator:235,border:235,scrollbar:235
--color info:243,prompt:223,spinner:167,pointer:167,marker:167,header:223
" 

# Prettify less
export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;33m'
export LESS_TERMCAP_so=$'\e[01;44;37m'
export LESS_TERMCAP_us=$'\e[01;37m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_ue=$'\e[0m'
export GROFF_NO_SGR=1

# MacOS overrides
if [[ "$OSTYPE" == "darwin"* ]]; then
  export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git"
  alias ll="ls -GahlF"
fi

# Local configuration
if [[ -f "$HOME/.zshrc.local" ]]; then
  source "$HOME/.zshrc.local"
fi
