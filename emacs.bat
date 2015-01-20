set drive=C:

set emacs_home=%drive%\Soft\Emacs\emacs-24.3

set dropbox=C:/oleg/Dropbox

set cygwin_home=C:\cygwin
set CYGWIN=nodosfilewarning 

set utils=%drive%\utils

set path=%utils%;%cygwin_home%\bin;%utils%\7-Zip;%utils%\gnu-win32;%utils%\putty;%path%
set PATH=c:/soft/Git/bin/;%ORACLE_HOME%\instantclient_11_2;%PATH%

rem set CLASSPATH=%CLASSPATH%;%home%/emacs/bin;%home%/emacs/bin/blancosqlformatter-0.1.1.jar
rem set CLASSPATH=%CLASSPATH%;%xalan_home%/xalan.jar;%xalan_home%/serializer.jar;%xalan_home%/xml-apis.jar;%xalan_home%/xercesImpl.jar
set XALAN_HOME=c:/soft/xalan-j_2_7_1
set CLASSPATH=%CLASSPATH%;%xalan_home%/*
rem set CLASSPATH=%CLASSPATH%;%SAXON_HOME%/saxon9he.jar
rem set CLASSPATH=%CLASSPATH%;%CXF_HOME%\lib\cxf-manifest.jar

rem c:\utils\FtpUse\ftpuse.exe L: helablinux02 Customer /USER:operator

set NAGRA=yes
set J2EP_DEBUG=c:\temp\j2ep_debug.log


set JAVA_HOME=C:\java\jdk1.7.0_71
set ANT_HOME=C:\ant\1.9NAGRA4.0
PATH %JAVA_HOME%\bin;%ANT_HOME%\bin;%PATH%


%emacs_home%\bin\runemacs.exe --debug-init
rem %emacs_home%\bin\runemacs.exe -q

