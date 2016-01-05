#!/bin/bash 
export HISTCONTROL=ignoredups

# the following two lines makes the bash ignore the windows returns \r
# to avoid errors like line : $'\r': command not found
export SHELLOPTS
set -o igncr

# function p4 {
#     export PWD=`cygpath -wa .`
#     /cygdrive/c/Program\ Files\ \(x86\)/Perforce/p4.exe $@
# }
           
PS1='$PWD>'


