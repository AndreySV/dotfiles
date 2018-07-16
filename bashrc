# Hey Emacs, this is -*- shell-script -*-
#
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
# If not running interactively, don't do anything
[ -z "$PS1" ] && return



# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac


# don't put duplicate lines in the history. See bash(1) for more options
#export HISTCONTRO=ignoredups
# ... and ignore same sucessive entries.
#export HISTCONTROL=ignoreboth
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=100000
export HISTFILESIZE=$HISTSIZE
export HISTIGNORE="&:[ ]*:exit:ls:cd ..:make:"
export HISTTIMEFORMAT="%h %d %H:%M:%S >  "

# Append to the Bash history file, rather than overwriting it
shopt -s histappend

# update command history immediately after executing new command
UPDATE_HISTORY_COMMAND="history -a; history -c; history -r"
export PROMPT_COMMAND="$UPDATE_HISTORY_COMMAND; $PROMPT_COMMAND"



# disable XON/XOFF control for terminal. 
# It allows to work forware-history-search by pressing Ctrl-S.
stty -ixon

# Case-insensitive globbing (used in pathname expansion)
# shopt -s nocaseglob

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
#shopt -s checkwinsize
# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h: '
fi

# add information about current git branch
export PS1+='$(__git_ps1 "(git)-[%s] ")\$ '

unset color_prompt force_color_prompt


# ccache settings
# ccache -M 4G
CCACHE_DIR=~/data/linux/.ccache
export CCACHE_DIR

export ALTERNATE_EDITOR=""
export EDITOR="edit.sh"

# hosts aliases
export HOSTALIASES=~/.hosts

# definitions for Debian tools
DEBEMAIL="andrej.skvortzov@gmail.com"
DEBFULLNAME="Andrey Skvortsov"
export DEBEMAIL DEBFULLNAME

alias dquilt="quilt --quiltrc=${HOME}/.quiltrc-dpkg"
complete -F _quilt_completion $_quilt_complete_opt dquilt


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi



# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

umask 0022

# for pythonbrew
[[ -s "$HOME/.pythonbrew/etc/bashrc" ]] && source "$HOME/.pythonbrew/etc/bashrc"

# load host specific bash settings
if [ -f ~/.bashrc_host ]; then
    . ~/.bashrc_host
fi
