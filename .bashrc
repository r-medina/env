# $HOME/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=200000
export PROMPT_COMMAND='history -a'

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]
then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]
then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null
    then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

case "$TERM" in
    xterm-color)    color_prompt=yes;;
    xterm-16color)  color_prompt=yes;;
    xterm-256color) color_prompt=yes;;
esac

if [ "$color_prompt" = yes ]
then
    # shortens bash prompt
    PS1='\[\033[01;32m\]r\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='\u:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u: \w\a\]$PS1"
        ;;
    *)
        ;;
esac

# enable color support of ls and also add handy aliases
# only on linux
if [ -x /usr/bin/dircolors ]
then
    test -r $HOME/.dircolors && eval "$(dircolors -b $HOME/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# $HOME/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f $HOME/.bash_aliases ]
then
    . $HOME/.bash_aliases
fi


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix
then
    . /etc/bash_completion
fi

# alias to open up chrome
alias chrome='open -a Google\ Chrome'

# alias to start tor
alias tor='/Applications/TorBrowser_en-US.app/Contents/MacOS/tor'

# alias for anonymous browsing
alias torchrome='chrome --args --proxy-server="socks=127.0.0.1:9050;sock4=127.0.0.1:9050;sock5=127.0.0.1:9050" --incognito check.torproject.org &'

# make less do syntax highlighting
export LESSOPEN="| /usr/local/bin/src-hilite-lesspipe.sh %s"
export LESS=' -R '

# open ableton
alias ableton='open -a Ableton\ Live\ 8'

# lock screen
alias lock='/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend'

# alias ImageJ to open
alias imagej='open -a ImageJ'

# QuickLook alias
alias prev='qlmanage -p'

# checks if there is an emacs daemon running: yes - open a client with the file, no -
# start a daemon, open client
#
# did this because i only use emacs in terminal and dont want
# emacsclient <file_name> to open file in an open frame
#
# caution: even though all arguments passed via `${@:1}`, `emacsclient` only opens
# one file, unlike `emacs`
#
# excluding `grep` from `ps`: http://superuser.com/questions/409655/excluding-grep-from-process-list
# mass arguments: http://wiki.bash-hackers.org/scripting/posparams#mass_usage
function emacssmart {
    # `ps -e` - print all processes
    # `grep '[e]macs --daemon'` - greps out matching processes except itself
    if [[ $(ps -e | grep '[e]macs --daemon') ]]
    then
        # if daemon is running
        emacsclient -nw -c ${@:1} # windowless client, new one, pass all args (but
                                  # only one file will open)
    else
        # no daemon
        \emacs --daemon; emacssmart ${@:1} # start daemon, recursively call
                                           # self. fuck performance
    fi
}

# # UNCOMMENT FOR EMACS DAEMON ON LOGIN
# # launches emacs on login
# if [[ ! $(ps -e | grep '[e]macs --daemon') ]]
# then
#     \emacs --daemon
# fi

# make directory, cd in
function mkcd {
    if [ -n "$1" ]
    then
        mkdir -p $1 && cd $1
    else
        echo "usage: mkcd directory"
    fi
}

## htop by cpu usage
alias monitor="sudo htop --sort-key PERCENT_CPU"

alias CELLAR=/usr/local/Cellar

# for python virtualenv shit
# source /usr/local/bin/virtualenvwrapper.sh

# ruby ish
# not my favorite way to have ruby, but maintains most recent version
export PATH=$HOME/.rbenv/bin:$PATH
eval "$(rbenv init -)"

# git auto complete
source /usr/local/etc/bash_completion.d/git-completion.bash

# go things
export GOPATH=$HOME/code/go
export PATH=$GOPATH/bin:$PATH
export GOROOT=$CELLAR/go/1.3/libexec
export PATH=$PATH:$GOROOT/bin
export CGO_ENABLED=0
source /usr/local/etc/bash_completion.d/go-completion.bash

# node env
export NODE_ENV=development

export PATH=$HOME/bowery/bin:$PATH

#alias bowery_dev='API_ADDR=10.0.0.15:3000 BROOME_ADDR=127.0.0.1:4000 ENV=development bowery'
alias bowery_dev='ENV=development bowery'
alias crosby_dev='ENV=development crosby'
