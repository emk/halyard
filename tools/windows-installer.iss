;; Use this file with Inno Setup, an excellent open source
;; installer builder for windows.  You probably want to edit
;; this using the ISTool GUI, and you'll need the ISPP
;; preprocessor module.

;; The executable which contains our version information.
#define MAIN_EXE "..\Win32\Bin\Halyard.exe"

;; Should we include debugging support? When this is set
;; to 0, we build regular installers. When this is set to
;; 1, we build installers which include the "_d" versions
;; of our executables as an option.
#define DEBUG 1

;; Extract some useful version fields--no need to duplicate.
#define COMPANY GetStringFileInfo(MAIN_EXE, COMPANY_NAME)
#define VERSION GetFileVersion(MAIN_EXE)
#define VERNAME GetStringFileInfo(MAIN_EXE, FILE_DESCRIPTION)
#define COPYRIGHT GetStringFileInfo(MAIN_EXE, LEGAL_COPYRIGHT)

[Setup]
AppID={{59979E48-B18E-4878-9974-3A5D9ACE78E3}
MinVersion=4.1.1998,5.0.2195
AppCopyright={#COPYRIGHT}
AppName=Halyard
DefaultGroupName=Halyard
ShowLanguageDialog=yes
LicenseFile=..\LICENSE.txt
AppVerName={#VERNAME}
DefaultDirName={pf}\Halyard
AppPublisher={#COMPANY}
AppPublisherURL=http://iml.dartmouth.edu/
OutputDir=..\Win32\Bin
OutputBaseFilename=Halyard Setup {#VERSION}
[Files]
Source: ..\Win32\Bin\libmzgc2.dll; DestDir: {app}; Components: release
Source: ..\Win32\Bin\libmzsch2.dll; DestDir: {app}; Components: release
Source: ..\Win32\Bin\Halyard.exe; DestDir: {app}; Components: release
Source: ..\Win32\Bin\wxref_gl.dll; DestDir: {app}; Components: release
Source: ..\Win32\Bin\wxref_soft.dll; DestDir: {app}; Components: release
Source: ..\Win32\Bin\dbghelp.dll; DestDir: {app}
Source: ..\Win32\Bin\zlib.dll; DestDir: {app}
Source: ..\Win32\Bin\curl.exe; DestDir: {app}
Source: ..\Release-Notes.txt; DestDir: {app}; Components: release
Source: ..\LICENSE.txt; DestDir: {app}; Components: release
Source: ..\wx\MSVC\wx5L\5L.prefs; DestDir: {app}
Source: ..\Common\Runtime\*; DestDir: {app}\Runtime\; Flags: recursesubdirs; Components: release; Excludes: CVS,compiled
#if DEBUG
Source: ..\Win32\Bin\libmzgc2_d.dll; DestDir: {app}; Components: debug
Source: ..\Win32\Bin\libmzsch2_d.dll; DestDir: {app}; Components: debug
Source: ..\Win32\Bin\Halyard_d.exe; DestDir: {app}; Components: debug
#endif
[Icons]
Name: {group}\Halyard; Filename: {app}\Halyard.exe; WorkingDir: {app}; Comment: Halyard multimedia authoring system; IconIndex: 0; Flags: createonlyiffileexists
Name: {group}\Halyard (Debug); Filename: {app}\Halyard_d.exe; WorkingDir: {app}; Comment: Halyard multimedia authoring system (debugging build); IconIndex: 0; Flags: createonlyiffileexists
Name: {group}\Release Notes; Filename: {app}\Release-Notes.txt
[Components]
Name: release; Description: Halyard authoring tools; Flags: fixed; Types: regular custom
#if DEBUG
Name: debug; Description: Halyard authoring tools (debug versions)
#endif
[Types]
Name: regular; Description: Regular Install
Name: custom; Description: Custom Install; Flags: iscustom
