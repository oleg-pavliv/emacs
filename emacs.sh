#export drive=/media/disk
#export HOME=$drive/home
export emacs_home=/usr/share/emacs

export XALAN_HOME=/home/oleg/work/java/xalan-j_2_7_1

export CLASSPATH=$CLASSPATH:/home/oleg/emacs/bin:/home/oleg/emacs/bin/blancosqlformatter-0.1.1.jar
export CLASSPATH=$CLASSPATH:$XALAN_HOME/xalan.jar:$XALAN_HOME/serializer.jar:$XALAN_HOME/xml-apis.jar:$XALAN_HOME/xercesImpl.jar

export TEMP=~/temp

/usr/bin/emacs --debug-init



