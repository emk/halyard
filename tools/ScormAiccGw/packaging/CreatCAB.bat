@echo off

rem
rem Make sure to have cabarc in the path.
rem These next 5 lines can be removed.
rem

if (%1) == () goto start
path=%1;%path%
if (%2) == () goto start
path=%2;%path%
:start

rem
rem Create the CAB file with CABARC
rem
rem The '-s 6144' option reserves 6K bytes for signing.
rem The 'n' command creates a new cab file.
rem The 'l' command lists the cab contents.
rem

if not exist ScormAiccGw.cab goto makecab
del ScormAiccGw.cab

:makecab
@echo on
cabarc -s 6144 n ScormAiccGw.cab ..\control\ReleaseMinDependency\ScormAiccGw.dll SAG.inf
@echo off
if not exist ScormAiccGw.cab goto error
cabarc l ScormAiccGw.cab

rem
rem Sign the CAB file with SIGNCODE
rem
@echo on
signcode
@echo off
goto exit

:error
echo.
echo Error: CAB file not created
echo.

:exit
pause