#!/bin/bash 
export HISTCONTROL=ignoredups

# the following two lines makes the bash ignore the windows returns \r
# to avoid errors like line : $'\r': command not found
export SHELLOPTS
set -o igncr

PS1='$PWD>'


