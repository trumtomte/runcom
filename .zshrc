# History
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help:ll:n:nnn"

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

source $HOME/.exports
source $HOME/.aliases

PROMPT="%{$fg[blue]%}%n%{$fg[yellow]%}:%{$fg[blue]%}%m %{$fg[yellow]%}%~ %{$fg[yellow]%}λ%{$reset_color%} "
# Show nnn's current depth (level)
[ -n "$NNNLVL" ] && PROMPT="nnn($NNNLVL) $PROMPT"

# menu selection, case insensitive match, list all files
zstyle ':completion:*'  menu select
zstyle ':completion:*'  matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*'  file-list all

bindkey '^[[1;5D' vi-backward-blank-word
bindkey '^[[1;5C' vi-forward-blank-word
bindkey '^X^E' edit-command-line
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'l' vi-forward-char

# Local configuration
if [[ -f "$HOME/.zshrc.local" ]]; then
    source $HOME/.zshrc.local
fi
