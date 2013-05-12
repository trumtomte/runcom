autoload -U colors compinit promptinit zmv
colors && compinit && promptinit

zstyle ':completion:*' menu select

PROMPT="%{$fg[yellow]%}➜ "
RPROMPT="%{$fg[green]%}%~ %{$fg[black]%}| %{$fg[red]%}%* %{$reset_color%}"

export PATH=/usr/local/bin:/usr/local/sbin:/usr/local:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/local/mysql/bin:/usr/local/git/bin:/usr/texbin
export EDITOR='vim'
export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8

bindkey -v 
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward 

setopt AUTO_CD

alias ls='ls -G'
alias ll='ls -ahlG'
alias music='ncmpcpp'
alias localip='ifconfig en1 | grep inet'
alias externalip='curl ifconfig.me/ip'
alias tmux='TERM=screen-256color-bce tmux'

alias composer='composer_func'
alias htaccess='htaccess_func'
alias today='today_func'
alias cext='change_extensions_func'
alias addvhost='add_vhost_func'

alias hamlwatch='ruby /Users/sebbe/hamlwatcher.rb'

# Install and run Composer (generate .htaccess file if none exists)
composer_func() {
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
htaccess_func() {
    echo "Creating .htaccess..."
    echo "<IfModule mod_rewrite.c>
        RewriteEngine On
        RewriteCond %{REQUEST_FILENAME} !-d
        RewriteCond %{REQUEST_FILENAME} !-f
        RewriteRule ^(.*)$ index.php/\$1 [L]
    </IfModule>" > .htaccess
}
# Add a virtualhost to MAMP apache conf
add_vhost_func() {
    echo "# VHost for $1
<VirtualHost *>
   DocumentRoot \"/Users/sebbe/www/$1\"
   ServerName $1.localhost
</VirtualHost>\n" >> /Applications/MAMP/conf/apache/httpd.conf
}
# Echo calendar - highlights today
today_func() {
    cal_head=`cal | head -1`;
    cal_tail=`cal | tail -7`;
    today=`date "+%e"`;
    date "+%A %b %d, Week %V"
    echo "$cal_head";
    echo "${cal_tail/${today}/\033[1;34m${today}\033[0m}";
}
# Changes the extension of all files
change_extensions_func() {
    for f in *.$1; do base=`basename $f .$1`; mv $f $base.$2; done
}
