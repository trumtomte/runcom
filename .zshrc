# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="sebbe"

alias music='ncmpcpp'
alias localip='ifconfig en1 | grep inet'
alias externalip="echo IP: `curl -s http://checkip.dyndns.org/ | sed 's/[a-zA-Z<>/ :]//g'`"
alias getcomposer='~/getcomposer.sh'
alias makehtaccess='~/makehtaccess.sh'
alias lsa='ls -ahl'
alias lp='ls++'
alias lpa='ls++ -a'
alias tmux="TERM=screen-256color-bce tmux"
alias mc="mc --colors base_color=lightgray,default:normal=lightgray,default:selected=black,green:marked=yellow,default:markselect=white,green:errors=white,red:menu=lightgray,default:reverse=black,lightgray:dnormal=white,default:dfocus=black,green:dhotnormal=brightgreen,default:dhotfocus=brightgreen,green:viewunderline=brightred,default:menuhot=yellow,default:menusel=white,black:menuhotsel=yellow,black:helpnormal=black,lightgray:helpitalic=red,lightgray:helpbold=blue,lightgray:helplink=black,cyan:helpslink=yellow,default:gauge=white,black:input=black,green:directory=white,default:executable=brightgreen,default:link=brightcyan,default:stalelink=brightred,default:device=brightmagenta,default:core=red,default:special=black,default:editnormal=lightgray,default:editbold=yellow,default:editmarked=black,cyan:errdhotnormal=yellow,red:errdhotfocus=yellow,lightgray"

plugins=(git)

source $ZSH/oh-my-zsh.sh

export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:/usr/local/mysql/bin
export EDITOR="vim"

bindkey -v 

# vi style incremental search
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward 

setopt AUTO_CD

