# History
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help:ll:n:nnn"

setopt auto_cd              # cd by just typing paths
setopt prompt_subst         # prompts for parameter expansions, etc.
setopt no_match             # errors for non matching filenames
setopt inc_append_history   # continously add to history
setopt extended_history     # add timestamp and elapsed time
setopt hist_ignore_dups     # ignore duplicate previous commands
setopt no_case_glob         # case insensitive globbing
setopt no_beep              # silence!

autoload -Uz compinit colors edit-command-line
# Completion system and colornames (for the prompt below)
compinit && colors

# Edit command line widget
zle -N edit-command-line

source ~/.exports
source ~/.aliases

if [[ -f "~/.exports.local" ]]; then
    source ~/.exports.local
fi

function setprompt() {
    PROMPT="%{$fg[blue]%}%n %{$fg[yellow]%}λ "
    RPROMPT=" %{$fg[blue]%}%~ %{$fg[yellow]%}%m"
}

function precmd() {
    setprompt
}

# Set the custom prompt
setprompt

# Menu select, case insensitive, default coloring and custom for `options´
zstyle ':completion:*'              menu select
zstyle ':completion:*'              matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*'              list-colors ''
zstyle ':completion:*:options'      list-colors '=^(-- *)=32'

# Emacs mode
bindkey -e
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward 
bindkey '^X^E' edit-command-line
