# ~/.bash_aliases: executed by bash(1) for non-login shells from ~/.bashrc
 

# management aliases commands
alias reload="source ~/.bash_aliases"
alias ea="$EDITOR ~/.bash_aliases && reload"

# get most often used commands
alias most_used_commands="history | awk '{ \$1=\"\"; \$2=\"\"; \$3=\"\"; \$4=\"\"; \$5=\"\"; print }' | awk '{a[\$0]++}END{for(i in a){print a[i] \" \" i}}' | sort -rn | head -n20"
    

# some more ls aliases
alias ll='ls -lh'
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
alias ps='ps -A'
alias c='clear'

alias m='make'
alias mp='make program' 

# Processes
alias tu='htop --sort-key PERCENT_CPU'    # cpu
alias tm='htop --sort-key PERCENT_MEM'    # memory


# Git
alias g='git status'
alias gd='git diff'
alias gc='git commit -v'
alias gb='git branch'

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

