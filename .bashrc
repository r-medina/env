# $HOME/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=
export HISTFILESIZE=
# appends history after each command
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# if it can handle colors, set variable color_prompt
case "$TERM" in
    xterm-color)
        color_prompt=yes
        ;;
    xterm-16color)
        color_prompt=yes
        ;;
    xterm-256color)
        color_prompt=yes
        ;;
esac

# sets PS1
if [ "$color_prompt" = yes ]
then
    # shortens bash prompt + gives colors
    PS1='\[\033[01;32m\]r\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='\u:\w\$ '
fi
unset color_prompt

# Alias definitions.
# You may want to put all your additions into a separate file like
# $HOME/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f $HOME/.bash_aliases ]
then
    . $HOME/.bash_aliases
fi

# idk
# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix
then
    . /etc/bash_completion
fi

# checks if there is an emacs daemon running: yes - open a client with the file, no -
# start a daemon, open client
#
# did this because i only use emacs in terminal and dont want
# emacsclient <file_name> to open file in an open frame
#
# caution: even though all arguments passed via `${@:1}`, `emacsclient` only opens
# one file, unlike `emacs`
#
# excluding `grep` from `ps`:
#     http://superuser.com/questions/409655/excluding-grep-from-process-list
# mass arguments:
#     http://wiki.bash-hackers.org/scripting/posparams#mass_usage
function emacssmart {
    # `ps -e` - print all processes
    # `grep '[e]macs --daemon'` - greps out matching processes except itself
    if [[ $(ps -e | grep '[e]macs --daemon') ]]
    then
        # if daemon is running
        emacsclient -nw -c ${@:1} # windowless client, new one, pass all args (but
                                  # only one file will open)
    else # no daemon
        \emacs --daemon --chdir $HOME
        # start daemon, recursively call self. fuck performance
        emacssmart ${@:1}
    fi
}

# launches emacs on login
if [[ ! $(ps -e | grep '[e]macs --daemon') ]]
then
    \emacs --daemon --chdir $HOME 1>$HOME/.emacs.d/init.log 2>&1
fi

# make directory, cd in
function mkcd {
    if [ -n "$1" ]
    then
        mkdir -p $1 && cd $1
    else
        echo "usage: mkcd directory"
    fi
}

# node env
export NODE_ENV=development


# OS X SPECIFIC

# make less do syntax highlighting
export LESSOPEN="| /usr/local/bin/src-hilite-lesspipe.sh %s"
export LESS=' -R '

# alias to open up chrome
alias chrome='open -a Google\ Chrome'

# alias to start tor
alias tor='/Applications/TorBrowser_en-US.app/Contents/MacOS/tor'

# alias for anonymous browsing
alias torchrome='chrome --args --proxy-server="socks=127.0.0.1:9050;sock4=127.0.0.1:9050;sock5=127.0.0.1:9050" --incognito check.torproject.org &'

# open ableton
alias ableton='open -a Ableton\ Live\ 8'

# lock screen
alias lock='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'

# alias ImageJ to open
alias imagej='open -a ImageJ'

# QuickLook alias
alias prev='qlmanage -p'

## htop by cpu usage
alias monitor='sudo htop --sort-key PERCENT_CPU'

CELLAR='/usr/local/Cellar'

# for python virtualenv shit
source /usr/local/bin/virtualenvwrapper.sh

# ruby ish
# not my favorite way to have ruby, but maintains most recent version
PATH=$HOME/.rbenv/bin:$PATH
eval "$(rbenv init -)"

# git auto complete
source /usr/local/etc/bash_completion.d/git-completion.bash

# go things. it should be installed
GOVER=$(go version | grep -o '[0-9]\+\(\.[0-9]\+\)\+') # matches one or more numbers
                                                       # followed by one or more
                                                       # dot-then-numbers. i.e 12.3.45
export GOPATH=$HOME/code/go
PATH=$GOPATH/bin:$PATH
export GOROOT=$CELLAR/go/$GOVER/libexec
PATH=$PATH:$GOROOT/bin
export CGO_ENABLED=0

PATH=$HOME/bowery/bin:$PATH

#alias bowery_dev='API_ADDR=10.0.0.15:3000 BROOME_ADDR=127.0.0.1:4000 ENV=development bowery'
alias bowery_dev='ENV=development bowery'
alias crosby_dev='ENV=development crosby'
