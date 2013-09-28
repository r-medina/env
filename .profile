# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
umask 077

export EDITOR='emacs'

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# -n returns true if the string is not null
# -f returns true if the file right after exists

# set PATH so it includes user's private bin if it exists. "Prepends"
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:/usr/local/sbin:$PATH"
fi

# add matlab binaries folder to PATH variable
PATH="/Applications/MATLAB_R2010b.app/bin:$PATH"

# add mysql binaries
#PATH="/usr/local/mysql/bin:$PATH"

# add pip paths
PATH="/usr/local/share/python:$PATH"
PATH="/usr/local/share/npm/bin:$PATH"

# -d returns true if the directory right after exists

#/usr/bin/keychain -q $HOME/.ssh/id_rsa --host $USER
#. $HOME/.keychain/${USER}-sh

   test -r /sw/bin/init.sh && . /sw/bin/init.sh
