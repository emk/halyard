# Microsoft Developer Studio Project File - Name="FiveL" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=FiveL - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "FiveL.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "FiveL.mak" CFG="FiveL - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "FiveL - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "FiveL - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "FiveL - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\Bin"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GR /GX /O2 /I "..\..\..\Common" /I "..\..\..\Common\freetype2\include" /I "..\..\..\Common\libs\crypto" /I "\dev\Quicktime\QT501SDK\SDK\CIncludes" /I "..\..\DibLib" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib qtmlclient.lib msimg32.lib ws2_32.lib /nologo /subsystem:windows /machine:I386 /nodefaultlib:"libcmt.lib" /libpath:"Release" /libpath:"..\Bin" /libpath:"\dev\Quicktime\QT501SDK\SDK\Libraries"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "FiveL - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GR /GX /ZI /Od /I "..\..\..\Common" /I "..\..\..\Common\freetype2\include" /I "..\..\..\Common\libs\crypto" /I "\dev\Quicktime\QT501SDK\SDK\CIncludes" /I "..\..\DibLib" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /FR /Yu"stdafx.h" /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib qtmlclient.lib msimg32.lib ws2_32.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libcmt.lib" /out:"..\..\Bin\FiveL_d.exe" /pdbtype:sept /libpath:"Debug" /libpath:"..\BinD" /libpath:"\dev\Quicktime\QT501SDK\SDK\Libraries"
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "FiveL - Win32 Release"
# Name "FiveL - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\FiveL\Audio.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Card.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Config.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Dib.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\FiveL.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\FiveL.rc
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Graphics.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Header.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Input.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LBrowser.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LCommandKey.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LCursor.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LDiskUtils.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LFileBundle.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LFiles.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LFont.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LHttp.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LPalette.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LPicture.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LQuickTime.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LRegistry.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LResource.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LTouchZone.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LUtil.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Macro.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\QTGraphic.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\StdAfx.cpp

!IF  "$(CFG)" == "FiveL - Win32 Release"

!ELSEIF  "$(CFG)" == "FiveL - Win32 Debug"

# ADD CPP /Yc

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\FiveL\SysInfo.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Video.cpp
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\View.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\FiveL\Audio.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Card.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Config.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Debug.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Dib.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\FiveL.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Globals.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Graphics.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Header.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Input.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LBrowser.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LCommandKey.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LCommon.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LCursor.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LDiskUtils.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LFileBundle.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LFiles.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LFont.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LHttp.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LPalette.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LPicture.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LQuickTime.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LRegistry.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LResource.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LTouchZone.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\LUtil.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Macro.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\QTGraphic.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\resource.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\SingleInstance.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\StdAfx.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\SysInfo.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\TWin5LInterpreter.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Video.h
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\View.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=..\..\FiveL\backup_c.cur
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\cursor2.cur
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\FiveL.ico
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\hand.cur
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\left_cur.cur
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\Magnify.cur
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\right_cu.cur
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\small.ico
# End Source File
# Begin Source File

SOURCE=..\..\FiveL\up_curso.cur
# End Source File
# End Group
# End Target
# End Project
