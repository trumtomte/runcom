# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sebbe"

alias music='ncmpcpp'
alias localip='ifconfig en1 | grep inet'
alias externalip="echo IP: `curl -s http://checkip.dyndns.org/ | sed 's/[a-zA-Z<>/ :]//g'`"
alias lsa='ls -ahl'
alias lp='ls++'
alias lpa='ls++ -a'
alias tmux="TERM=screen-256color-bce tmux"
alias mc="mc --colors base_color=lightgray,default:normal=lightgray,default:selected=black,green:marked=yellow,default:markselect=white,green:errors=white,red:menu=lightgray,default:reverse=black,lightgray:dnormal=white,default:dfocus=black,green:dhotnormal=brightgreen,default:dhotfocus=brightgreen,green:viewunderline=brightred,default:menuhot=yellow,default:menusel=white,black:menuhotsel=yellow,black:helpnormal=black,lightgray:helpitalic=red,lightgray:helpbold=blue,lightgray:helplink=black,cyan:helpslink=yellow,default:gauge=white,black:input=black,green:directory=white,default:executable=brightgreen,default:link=brightcyan,default:stalelink=brightred,default:device=brightmagenta,default:core=red,default:special=black,default:editnormal=lightgray,default:editbold=yellow,default:editmarked=black,cyan:errdhotnormal=yellow,red:errdhotfocus=yellow,lightgray"
alias composer='cmpsr'

plugins=(git)

source $ZSH/oh-my-zsh.sh

export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:/usr/local/mysql/bin:/usr/local/git/bin
export EDITOR="vim"

bindkey -v 

# vi style incremental search
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward 

setopt AUTO_CD

cmpsr() {
    # Get composer
    echo "Fetching composer..."
    curl -s http://getcomposer.org/installer | php
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

                    RewriteRule ^(.*)$ index.php/$1 [L]
                </IfModule>" > .htaccess
            fi
            echo "Done..."
        fi
    fi
}


