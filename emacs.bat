set drive=C:

set emacs_home=%drive%\Soft\Emacs\emacs-24.5

set dropbox=C:/oleg/Dropbox

set cygwin_home=c:/cygwin64
set CYGWIN=nodosfilewarning
set utils=%drive%/utils

set path=%utils%;%cygwin_home%\bin;%utils%\7-Zip;%utils%\gnu-win32;%utils%\putty;%path%
set PATH=c:/Soft/GnuWin32/bin/;c:/soft/Git/bin/;%ORACLE_HOME%\instantclient_11_2;%PATH%
set PATH=c:\Program Files\Microsoft SQL Server\110\Tools\Binn\;%PATH%

rem set CLASSPATH=%CLASSPATH%;%home%/emacs/bin;%home%/emacs/bin/blancosqlformatter-0.1.1.jar

set XALAN_HOME=c:/soft/java/xalan-j_2_7_1
set GRAPHVIZ_HOME=%utils%\graphviz-2.36
set CLASSPATH=%CLASSPATH%;%xalan_home%/xalan.jar;%xalan_home%/serializer.jar;%xalan_home%/xml-apis.jar;%xalan_home%/xercesImpl.jar
set CLASSPATH=%CLASSPATH%;%xalan_home%/*
rem set CLASSPATH=%CLASSPATH%;%SAXON_HOME%/saxon9he.jar
rem set CLASSPATH=%CLASSPATH%;%CXF_HOME%\lib\cxf-manifest.jar

rem c:\utils\FtpUse\ftpuse.exe L: helablinux02 Customer /USER:operator
c:\utils\FtpUse\ftpuse.exe O: chx-osh-01 opaadmin /USER:adminopa


set NAGRA=yes
rem set J2EP_DEBUG=c:\temp\j2ep_debug.log


rem set JAVA_HOME=C:\java\jdk1.7.0_71
rem set JAVA_HOME=C:\Soft\java\jdk1.6.0_43
rem set JAVA_HOME=C:\Soft\java\jdk1.8.0_31

rem set ANT_HOME=C:\ant\1.9NAGRA4.0
rem PATH %JAVA_HOME%\bin;%ANT_HOME%\bin;%PATH%

%emacs_home%\bin\runemacs.exe --debug-init
rem %emacs_home%\bin\runemacs.exe -q

