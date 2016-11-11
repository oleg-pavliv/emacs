set drive=C:

set emacs_home=%drive%\Soft\Emacs

set cygwin_home=C:\cygwin64
set CYGWIN=nodosfilewarning 

set utils=%drive%\onedrive\utils

set path=%utils%;%cygwin_home%\bin;%utils%\7-Zip;%utils%\gnu-win32;%utils%\putty;%path%
set PATH=%ORACLE_HOME%\instantclient_11_2;%PATH%
set PATH=C:\Soft\Python27;%PATH%

rem set CLASSPATH=%CLASSPATH%;%home%/emacs/bin;%home%/emacs/bin/blancosqlformatter-0.1.1.jar
rem set CLASSPATH=%CLASSPATH%;%xalan_home%/xalan.jar;%xalan_home%/serializer.jar;%xalan_home%/xml-apis.jar;%xalan_home%/xercesImpl.jar
set CLASSPATH=%CLASSPATH%;%xalan_home%/*
rem set CLASSPATH=%CLASSPATH%;%SAXON_HOME%/saxon9he.jar
rem set CLASSPATH=%CLASSPATH%;%CXF_HOME%\lib\cxf-manifest.jar


rem set IDTV_ORG=u:/Idtv3/org
rem set IDTV=IDTV_WINDOWS
set NAGRA=1

%drive%\Soft\emacs-24.5\bin\runemacs.exe --debug-init

