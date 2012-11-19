# ~/.bash_aliases: executed by bash(1) for non-login shells from ~/.bashrc
 
# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# Simple Commands
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias .......='cd ../../../../../..'
alias ........='cd ../../../../../../..'
alias .........='cd ../../../../../../../..'

# Shortcuts
alias e='ps -ef | grep'
alias g='grep'
alias o='popd'
alias p='pushd '
alias vi='vim'

alias d="cd ~/data/Dropbox"
alias dl="cd ~/data/Downloads"

# Enable aliases to be sudo’ed
alias sudo='sudo '

# Network utils
alias network.ports='netstat -a -n | grep -i "LISTEN "'
alias network.connections='lsof -l -i +L -R -V'
alias network.established='lsof -l -i +L -R -V | grep ESTABLISHED'

# Alias directories for quick access - links often work better but these are helpful
# alias tmp=~/data/tmp
alias tmp=~/tmp

