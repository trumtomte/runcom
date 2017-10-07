setopt correct
setopt prompt_subst

# History
setopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt EXTENDED_HISTORY

autoload -U colors compinit promptinit
colors && compinit && promptinit

. ~/.exports
. ~/.functions
. ~/.aliases

# set VIMODE according to the current mode (default “[i]”)
function zle-keymap-select zle-line-init zle-line-finish {
    setleftprompt
    zle reset-prompt
    zle -R
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

# Change the terminal circle color when in Vi mode
function vi_mode_prompt_info() {
    echo "${${KEYMAP/vicmd/green}/(main|viins)/red}"
}

# Current Git branch
function git_prompt_info() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(git rev-parse --short HEAD 2> /dev/null) || return
    echo "%{$fg[black]%}| %{$fg[yellow]%}${ref#refs/heads/}%{$reset_color%}"
}

function set_virtualenv_prompt() {
    if test -z "$VIRTUAL_ENV" ; then
        PYTHON_VIRTUALENV=""
    else
        PYTHON_VIRTUALENV="(`basename \"$VIRTUAL_ENV\"`) "
    fi
}

# Before commands, update promt and window title
function precmd() {
    set_virtualenv_prompt
    PROMPT="${PYTHON_VIRTUALENV}%{$fg[$(vi_mode_prompt_info)]%}🐕  "
    RPROMPT="%{$fg[green]%}%~ %{$fg[black]%}| %{$fg[red]%}%* %{$reset_color%}$(git_prompt_info)"
    echo -n -e "\033]0;${USER}@${HOST}\007"
}

function setleftprompt() {
    PROMPT="${PYTHON_VIRTUALENV}%{$fg[$(vi_mode_prompt_info)]%}🐕  "
    # PROMPT="%{$fg[$(vi_mode_prompt_info)]%}• %{$fg[yellow]%}➜ "
}
function setrightprompt() {
    RPROMPT="%{$fg[green]%}%~ %{$fg[black]%}| %{$fg[red]%}%* %{$reset_color%} $(git_prompt_info)"
}
function setprompt() {
    set_virtualenv_prompt
    setleftprompt
    setrightprompt
}
# Set the custom prompt
setprompt

zstyle ':completion:*' menu select                              # Menu select
zstyle ':completion::complete:*' use-cache 1                    # Cache
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'             # Case insensitive
zstyle ':completion:*' list-colors "=(#b) #([0-9]#)*=36=31"     # Color

# Vi mode
# bindkey -v 
# Emacs mode
bindkey -e
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward 
