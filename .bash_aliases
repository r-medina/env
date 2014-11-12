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

# for searching node packages
# excludes tons-o-shit
alias node_grep="grep --exclude-dir=node_modules --exclude-dir=.build --exclude-dir='./public/components'"

