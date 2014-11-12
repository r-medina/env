# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
# umask 077

export EDITOR='emacs'

# -n returns true if the string is not null
# -f returns true if the file right after exists

if [ -d "/usr/local/sbin" ]
then
    export PATH="/usr/local/sbin:$PATH"
fi

# if running bash
if [ -n "$BASH_VERSION" ]
then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]
    then
        . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists. "Prepends"
if [ -d "$HOME/bin" ]
then
    export PATH="$HOME/bin:$PATH"
fi

# {{{
# Node Completion - Auto-generated, do not touch.
shopt -s progcomp
for f in $(command ls ~/.node-completion)
do
    f="$HOME/.node-completion/$f"
    test -f "$f" && . "$f"
done
# }}}
