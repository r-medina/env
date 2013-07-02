#!/bin/bash 
#cron job for checking .emacs file and commiting to repository

if diff ./.emacs ../.emacs >/dev/null ; then
    :
else
    cp ../.emacs . 
    date >> log
    git add .emacs
    git commit -m "committing changes to .emacs" > /dev/null
    git push -u origin master > /dev/null
fi
