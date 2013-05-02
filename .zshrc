# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
ZSH_THEME='sebbe'

alias music='ncmpcpp'
alias localip='ifconfig en1 | grep inet'
alias externalip='curl ifconfig.me/ip'
alias lsa='ls -ahl'
alias tmux='TERM=screen-256color-bce tmux'
alias composer='cmpsr'
alias htaccess='htaccessfile'
alias minifyjs='minify_js'
alias today='today_func'
alias path='paths'
alias hamlwatch='ruby /Users/sebbe/hamlwatcher.rb'
alias cext='changeExts'
alias addvhost='add_site'

plugins=(git)

source $ZSH/oh-my-zsh.sh

export PATH=/usr/local/bin:/usr/local/sbin:/usr/local:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/local/mysql/bin:/usr/local/git/bin:/usr/texbin
export EDITOR='vim'
export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8

bindkey -v 
# vi style incremental search
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward 

setopt AUTO_CD

# Composer..
cmpsr() {
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
htaccessfile() {
    echo "Creating .htaccess..."
    echo "<IfModule mod_rewrite.c>
        RewriteEngine On
        RewriteCond %{REQUEST_FILENAME} !-d
        RewriteCond %{REQUEST_FILENAME} !-f
        RewriteRule ^(.*)$ index.php/\$1 [L]
    </IfModule>" > .htaccess
}
# Add a virtualhost to MAMP apache conf
add_site() {
    echo "# VHost for $1
<VirtualHost *>
   DocumentRoot \"/Users/sebbe/www/$1\"
   ServerName $1.localhost
</VirtualHost>\n" >> /Applications/MAMP/conf/apache/httpd.conf
}
# Minify JS
minify_js() {
    echo "Compressing $1 to $2 ..."
    java -jar $HOME/compiler.jar --js $1 --js_output_file $2 --logging_level INFO
    echo "Done!"
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
# Echo all $PATH:s in a more readable fashion
paths() {
    echo $PATH | tr ':' '\n';
}
# Changes the extension of all files
changeExts() {
    for f in *.$1; do base=`basename $f .$1`; mv $f $base.$2; done
}
