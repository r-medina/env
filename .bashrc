# ~/.bashrc: executed by bash(1) for non-login shells.
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
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

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

case "$TERM" in
    xterm-color)    color_prompt=yes;;
    xterm-16color)  color_prompt=yes;;
    xterm-256color) color_prompt=yes;;
esac

if [ "$color_prompt" = yes ]; then
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
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias ls='ls -G'

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
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# alias open='gnome-open'
alias uncut='cut --complement'

# alias to run matlab from the shell
alias matlab='matlab -nodesktop'

# alias to open up chrome
alias chrome='open -a Google\ Chrome'

# alias to start tor
alias tor='/Applications/TorBrowser_en-US.app/Contents/MacOS/tor &'

# alias for anonymous browsing
#alias torchrome='chrome --proxy-server="socks=127.0.0.1:9050;sock4=127.0.0.1:9050;sock5=127.0.0.1:9050" --incognito check.torproject.org &'
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

# privoxy alias
alias privoxy='/usr/local/Cellar/privoxy/3.0.21/sbin/privoxy /usr/local/etc/privoxy/config'

alias tree='tree -C'

# alias for smart emacs launching function
alias emacs='emacssmart'
alias emacskill="emacsclient -e '(kill-emacs)'"

# emacssmart checks to see if there is more than one emacs process
# ie: if there's a daemon and a frame
# if true, it opens a new emacs frame with the file
# did this because i only use emacs in terminal and dont want
# emacsclient <file_name> to open file in an open frame
function emacssmart {
    if [ "$(pgrep emacs | wc -l)" -ge 1 ]
    then
	emacsclient -c $1
    elif [ ! $(pgrep emacs) ]
    then
	if [ -z "$1" ]
	then 
	    \emacs --daemon
	else
	    emacssmart; emacssmart $1
	fi
    fi
}

# launches emacs on new window
if [[ ! $(pgrep emacs) ]]
then
    emacssmart
fi

# for virtualenv shit
source /usr/local/bin/virtualenvwrapper.sh

# ruby ish
# not my favorite way to have ruby, but maintains most recent version
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# git auto complete
source /usr/local/etc/bash_completion.d/git-completion.bash
