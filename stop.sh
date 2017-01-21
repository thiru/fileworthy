#!/bin/sh

# Stops the Fileworthy web server

screen -S fileworthy -X stuff '(fileworthy:stop-app)^M'
screen -S fileworthy -X stuff '(exit)^M'
screen -S fileworthy -X stuff 'exit^M'
