@echo off

rem
rem Batch file to create a test Software Publisher Certificate,
rem as well as a test company certificate and test private key.
rem

if (%1) == () goto usage

rem
rem Make sure to have makecert in the path.
rem
if (%2) == () goto start
path=%2;%path%
:start

rem
rem You can use the MAKECERT and CERT2SPC utilities provided in 
rem the CAB&SIGN directory on the Visual C++ 5.0 CD to make a 
rem test Software Publisher Certificate. 
rem
rem Note that this test SPC is not valid for software publishing, 
rem but can be used to test code signing. 
rem
rem To make a private key file called TestKey.PVK and a company 
rem certificate called Test.CER, run the MAKECERT utility with 
rem the following command:
rem

@echo on
makecert -u:TestKey -n:CN=IML -k:TestKey.PVK Test.cer
@echo off

rem
rem To create a test Software Publisher Certificate (SPC) called TEST.SPC, 
rem run the CERT2SPC utility with the following command:
rem
rem (This runs a wizard which installs the certificate.)  
rem

@echo on
CERT2SPC %1\root.cer test.cer test.spc
@echo off
goto end

:usage
echo.
echo usage: CreatSPC [root.cer directory] [makecert directory (optional)]
echo.

:end
pause
