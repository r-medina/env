#!/bin/bash 
# cron job for checking changes and committing repo

cd ~/env

if ! git status -s #>/dev/null
then
    :
else
    git add .
    git commit -m "auto message: committing changes to config files"
    git push -u origin master
fi
