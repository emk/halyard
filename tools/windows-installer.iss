;; The executable which contains our version information.
#define MAIN_EXE "..\Win32\Bin\Tamale.exe"

;; Extract some useful version fields--no need to duplicate.
#define COMPANY GetStringFileInfo(MAIN_EXE, COMPANY_NAME)
#define VERSION GetStringFileInfo(MAIN_EXE, FILE_DESCRIPTION)
#define COPYRIGHT GetStringFileInfo(MAIN_EXE, LEGAL_COPYRIGHT)

;; Should we include debugging support?
#define DEBUG 1

[Setup]
AppID={{59979E48-B18E-4878-9974-3A5D9ACE78E3}
MinVersion=4.1.1998,5.0.2195
AppCopyright={#COPYRIGHT}
AppName=Tamale
DefaultGroupName=Tamale
ShowLanguageDialog=yes
LicenseFile=..\LICENSE
AppVerName={#VERSION}
DefaultDirName={pf}\Tamale
AppPublisher={#COMPANY}
AppPublisherURL=http://iml.dartmouth.edu/
[Files]
Source: ..\Win32\Bin\libmzgc2.dll; DestDir: {app}; Components: release
Source: ..\Win32\Bin\libmzsch2.dll; DestDir: {app}; Components: release
Source: ..\Win32\Bin\Tamale.exe; DestDir: {app}; Components: release
Source: ..\Win32\Bin\wxref_gl.dll; DestDir: {app}; Components: release
Source: ..\Win32\Bin\wxref_soft.dll; DestDir: {app}; Components: release
Source: ..\Release-Notes.txt; DestDir: {app}; Components: release
Source: ..\LICENSE; DestDir: {app}; Components: release
Source: ..\Common\Runtime\*.ss; DestDir: {app}\Runtime\; Flags: recursesubdirs; Components: release
Source: ..\Common\Runtime\*.txt; DestDir: {app}\Runtime\; Flags: recursesubdirs; Components: release
Source: ..\Common\Runtime\*.gif; DestDir: {app}\Runtime\; Flags: recursesubdirs; Components: release
#if DEBUG
Source: ..\Win32\Bin\libmzgc2_d.dll; DestDir: {app}; Components: debug
Source: ..\Win32\Bin\libmzsch2_d.dll; DestDir: {app}; Components: debug
Source: ..\Win32\Bin\Tamale_d.exe; DestDir: {app}; Components: debug
#endif
[Icons]
Name: {group}\Tamale; Filename: {app}\Tamale.exe; WorkingDir: {app}; Comment: Tamale multimedia authoring system; IconIndex: 0; Flags: createonlyiffileexists
Name: {group}\Tamale (Debug); Filename: {app}\Tamale_d.exe; WorkingDir: {app}; Comment: Tamale multimedia authoring system (debugging build); IconIndex: 0; Flags: createonlyiffileexists
Name: {group}\Release Notes; Filename: {app}\Release-Notes.txt
[Components]
Name: release; Description: Tamale authoring tools; Flags: fixed; Types: regular custom
#if DEBUG
Name: debug; Description: Tamale authoring tools (debug versions)
#endif
[Types]
Name: regular; Description: Regular Install
Name: custom; Description: Custom Install; Flags: iscustom
