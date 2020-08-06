setopt prompt_subst
setopt no_case_glob
setopt auto_cd
setopt inc_append_history
setopt extended_history     # Add timestamp and elapsed time
setopt share_history        # Share history across sessions
setopt hist_ignore_dups     # Dont store duplicates

HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help"

autoload -Uz colors compinit promptinit edit-command-line
colors && compinit && promptinit

zle -N edit-command-line

. ~/.exports
. ~/.functions
. ~/.aliases

# Current Git branch
# function git_prompt_info() {
#     ref=$(git symbolic-ref HEAD 2> /dev/null) || \
#     ref=$(git rev-parse --short HEAD 2> /dev/null) || return
#     echo "%{$fg[black]%}| %{$fg[yellow]%}${ref#refs/heads/}%{$reset_color%}"
# }

function setleftprompt() {
    PROMPT="%{$fg[yellow]%}λ "
}

function setrightprompt() {
    # RPROMPT="%{$fg[green]%}%~ %{$fg[black]%}| %{$fg[red]%}%* %{$reset_color%} $(git_prompt_info)"
    RPROMPT="%{$fg[green]%}%~ %{$fg[black]%}| %{$fg[red]%}%"
}

function setprompt() {
    setleftprompt
    setrightprompt
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

# Edit the command line (bash style)
bindkey "^X^E" edit-command-line

# Installed via brew
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# TODO use asdf instead of nvm
. /usr/local/opt/asdf/asdf.sh

# Added by FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
