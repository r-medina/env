# :O colorful grep!
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias ls='ls -G'

# alias open='gnome-open'
alias uncut='cut --complement'

# alias to run matlab from the shell
alias matlab='matlab -nodesktop'

# colors
alias tree='tree -C'

# alias for smart emacs launching function
alias emacs='emacssmart'

# kill clients, kill daemon, let em know
# needs `;` after first command because pkill returns 1 if no processes
alias emacskill="pkill emacsclient; emacsclient -e '(kill-emacs)' && echo 'killed emacs daemon'"

# for finding a string in a directory
alias dir_grep="grep -HIrsn --exclude-dir=.git"

# for searching node packages
# excludes tons-o-shit
alias node_grep="grep --exclude-dir=node_modules \
                      --exclude-dir=.build \
                      --exclude-dir='./public/components'"

# new ssh tunnel to server
alias sshn='ssh -S none'
