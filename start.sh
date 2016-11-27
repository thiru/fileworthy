#!/bin/sh

# Starts the web server in a screen session:
# * detached by default
# * name of session is "fwweb"
# * session will not die even if sbcl process ends

screen -dmS fwweb sh -c 'sbcl --load start.lisp; exec zsh'
