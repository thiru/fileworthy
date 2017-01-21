#!/bin/sh

# Restart already running Hockey Oracle within screen session

screen -S fileworthy -X stuff '(fileworthy:stop-app)^M'
screen -S fileworthy -X stuff '(exit)^M'
screen -S fileworthy -X stuff 'sbcl --load restart.lisp^M'
