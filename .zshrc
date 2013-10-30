# Current Git branch
function git_prompt_info() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(git rev-parse --short HEAD 2> /dev/null) || return
    echo "%{$fg[black]%}| %{$fg[yellow]%}${ref#refs/heads/}%{$reset_color%}"
}

setopt correct
setopt prompt_subst
setopt INC_APPEND_HISTORY

HISTFILE=$HOME/.zhistory
HISTSIZE=99999
SAVEHIST=99999

autoload -U colors compinit promptinit
colors && compinit && promptinit

# set VIMODE according to the current mode (default “[i]”)
function zle-keymap-select zle-line-init zle-line-finish {
    setleftprompt
    zle reset-prompt
    zle -R
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

function vi_mode_prompt_info() {
    echo "${${KEYMAP/vicmd/green}/(main|viins)/red}"
}

function precmd() {
    # Update prompt
    RPROMPT="%{$fg[green]%}%~ %{$fg[black]%}| %{$fg[red]%}%* %{$reset_color%}$(git_prompt_info)"
    # Update window title
    echo -n -e "\033]0;${USER}@${HOST}\007"
}

function setleftprompt() {
    PROMPT="%{$fg[$(vi_mode_prompt_info)]%}• %{$fg[yellow]%}➜ "
}
function setrightprompt() {
    RPROMPT="%{$fg[green]%}%~ %{$fg[black]%}| %{$fg[red]%}%* %{$reset_color%} $(git_prompt_info)"
}

function setprompt() {
    setleftprompt
    setrightprompt
}
setprompt

# Menu select
zstyle ':completion:*' menu select
# Cache
zstyle ':completion::complete:*' use-cache 1
# Case Insensitive
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
# Color
zstyle ':completion:*' list-colors "=(#b) #([0-9]#)*=36=31"

export PATH=/usr/local/bin:/usr/local/sbin:/usr/local:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/local/mysql/bin:/usr/local/git/bin:/usr/texbin
export EDITOR='vim'
export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8

# Vi mode
bindkey -v 

bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward 

alias ls='ls -G'
alias ll='ls -ahlG'
alias music='ncmpcpp'
alias localip='ifconfig en1 | grep inet'
alias externalip='curl ifconfig.me/ip'
# alias tmux='TERM=screen-256color-bce tmux'
alias composer='composer_func'
alias htaccess='htaccess_func'
alias today='today_func'
alias cext='change_extensions_func'
alias addvhost='add_vhost_func'
alias hamlwatch='ruby /Users/sebbe/hamlwatcher.rb'
alias mampphp='/Applications/MAMP/bin/php/php5.4.4/bin/php'
alias apacheconf='vim /Applications/MAMP/conf/apache/httpd.conf'

# Install and run Composer (generate .htaccess file if none exists)
function composer_func() {
    # Get composer
    echo "Fetching composer..."
    curl -sS http://getcomposer.org/installer | php -d detect_unicode=off
    # Store error code
    err=$?
    # Did an error occur?
    if [ $err -eq 1 ]; then
        echo "Aborting..."
    else
        # Install composer
        echo "Install composer..."
        php composer.phar install
        # Store error code
        err=$?
        # Did an error occur?
        if [ $err -eq 1 ]; then
            echo "Aborting..."
        else
            # Get the file path
            file=$(pwd)/.htaccess
            echo "Checking if an htaccess file exists..."
            # Does the file exist?
            if [ -e $file ]; then
                echo "htaccess already exists..."
            else
                # Echo mod_rewrite to htaccess
                echo "Creating .htaccess..."
                echo "<IfModule mod_rewrite.c>
                    RewriteEngine On
                                    
                    RewriteCond %{REQUEST_FILENAME} !-d
                    RewriteCond %{REQUEST_FILENAME} !-f

                    RewriteRule ^(.*)$ index.php/\$1 [L]
                </IfModule>" > .htaccess
            fi
            echo "Done..."
        fi
    fi
}
# Creates an htaccess file 
function htaccess_func() {
    echo "Creating .htaccess..."
    echo "<IfModule mod_rewrite.c>
        RewriteEngine On
        RewriteCond %{REQUEST_FILENAME} !-d
        RewriteCond %{REQUEST_FILENAME} !-f
        RewriteRule ^(.*)$ index.php/\$1 [L]
    </IfModule>" > .htaccess
}
# Add a virtualhost to MAMP apache conf
function add_vhost_func() {
    echo "# VHost for $1
<VirtualHost *>
   DocumentRoot \"/Users/sebbe/www/$1\"
   ServerName $1.localhost
</VirtualHost>\n" >> /Applications/MAMP/conf/apache/httpd.conf
}
# Echo calendar - highlights today
function today_func() {
    cal_head=`cal | head -1`;
    cal_tail=`cal | tail -7`;
    today=`date "+%e"`;
    date "+%A %b %d, Week %V"
    echo "$cal_head";
    echo "${cal_tail/${today}/\033[1;34m${today}\033[0m}";
}
# Changes the extension of all files
function change_extensions_func() {
    for f in *.$1; do base=`basename $f .$1`; mv $f $base.$2; done
}
