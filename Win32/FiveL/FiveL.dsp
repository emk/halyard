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
# PROP Output_Dir "..\Bin"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\Common" /I "\dev\Quicktime\QT501SDK\SDK\CIncludes" /I "..\DibLib" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 diblib.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib qtmlclient.lib msimg32.lib ws2_32.lib /nologo /subsystem:windows /machine:I386 /nodefaultlib:"libcmt.lib" /out:"..\Bin/5L.exe" /libpath:"..\Bin" /libpath:"\dev\Quicktime\QT501SDK\SDK\Libraries"
# SUBTRACT LINK32 /nodefaultlib

!ELSEIF  "$(CFG)" == "FiveL - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\BinD"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\Common" /I "\dev\Quicktime\QT501SDK\SDK\CIncludes" /I "..\DibLib" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /FR /Yu"stdafx.h" /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 diblibd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib qtmlclient.lib msimg32.lib ws2_32.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libcmt.lib" /out:"..\BinD/5L_d.exe" /pdbtype:sept /libpath:"..\BinD" /libpath:"\dev\Quicktime\QT501SDK\SDK\Libraries"
# SUBTRACT LINK32 /nodefaultlib

!ENDIF 

# Begin Target

# Name "FiveL - Win32 Release"
# Name "FiveL - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\Audio.cpp
# End Source File
# Begin Source File

SOURCE=.\Card.cpp
# End Source File
# Begin Source File

SOURCE=.\Config.cpp
# End Source File
# Begin Source File

SOURCE=.\Dib.cpp
# End Source File
# Begin Source File

SOURCE=.\FiveL.cpp
# End Source File
# Begin Source File

SOURCE=.\FiveL.rc
# End Source File
# Begin Source File

SOURCE=.\Graphics.cpp
# End Source File
# Begin Source File

SOURCE=.\Header.cpp
# End Source File
# Begin Source File

SOURCE=.\Index.cpp
# End Source File
# Begin Source File

SOURCE=.\Input.cpp
# End Source File
# Begin Source File

SOURCE=.\LBrowser.cpp
# End Source File
# Begin Source File

SOURCE=.\LCommandKey.cpp
# End Source File
# Begin Source File

SOURCE=.\LCursor.cpp
# End Source File
# Begin Source File

SOURCE=.\LDiskUtils.cpp
# End Source File
# Begin Source File

SOURCE=.\LFiles.cpp
# End Source File
# Begin Source File

SOURCE=.\LFont.cpp
# End Source File
# Begin Source File

SOURCE=.\LHttp.cpp
# End Source File
# Begin Source File

SOURCE=.\LPalette.cpp
# End Source File
# Begin Source File

SOURCE=.\LPicture.cpp
# End Source File
# Begin Source File

SOURCE=.\LQuickTime.cpp
# End Source File
# Begin Source File

SOURCE=.\LQuickTime2.cpp
# End Source File
# Begin Source File

SOURCE=.\LResource.cpp
# End Source File
# Begin Source File

SOURCE=.\LStream.cpp
# End Source File
# Begin Source File

SOURCE=.\LTouchZone.cpp
# End Source File
# Begin Source File

SOURCE=.\LUtil.cpp
# End Source File
# Begin Source File

SOURCE=.\Macro.cpp
# End Source File
# Begin Source File

SOURCE=.\Parser.cpp
# End Source File
# Begin Source File

SOURCE=.\QTGraphic.cpp
# End Source File
# Begin Source File

SOURCE=.\StdAfx.cpp
# ADD CPP /Yc"stdafx.h"
# End Source File
# Begin Source File

SOURCE=.\SysInfo.cpp
# End Source File
# Begin Source File

SOURCE=.\Variable.cpp
# End Source File
# Begin Source File

SOURCE=.\Video.cpp
# End Source File
# Begin Source File

SOURCE=.\View.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\Audio.h
# End Source File
# Begin Source File

SOURCE=.\Card.h
# End Source File
# Begin Source File

SOURCE=.\Config.h
# End Source File
# Begin Source File

SOURCE=.\Debug.h
# End Source File
# Begin Source File

SOURCE=.\Dib.h
# End Source File
# Begin Source File

SOURCE=.\FiveL.h
# End Source File
# Begin Source File

SOURCE=.\Globals.h
# End Source File
# Begin Source File

SOURCE=.\Graphics.h
# End Source File
# Begin Source File

SOURCE=.\Header.h
# End Source File
# Begin Source File

SOURCE=.\Index.h
# End Source File
# Begin Source File

SOURCE=.\Input.h
# End Source File
# Begin Source File

SOURCE=.\LBrowser.h
# End Source File
# Begin Source File

SOURCE=.\LCommandKey.h
# End Source File
# Begin Source File

SOURCE=.\LCursor.h
# End Source File
# Begin Source File

SOURCE=.\LDiskUtils.h
# End Source File
# Begin Source File

SOURCE=.\LFiles.h
# End Source File
# Begin Source File

SOURCE=.\LFont.h
# End Source File
# Begin Source File

SOURCE=.\LHttp.h
# End Source File
# Begin Source File

SOURCE=.\LPalette.h
# End Source File
# Begin Source File

SOURCE=.\LPicture.h
# End Source File
# Begin Source File

SOURCE=.\LQuickTime.h
# End Source File
# Begin Source File

SOURCE=.\LQuickTime2.h
# End Source File
# Begin Source File

SOURCE=.\LResource.h
# End Source File
# Begin Source File

SOURCE=.\LStream.h
# End Source File
# Begin Source File

SOURCE=.\LTouchZone.h
# End Source File
# Begin Source File

SOURCE=.\LUtil.h
# End Source File
# Begin Source File

SOURCE=.\LVersion.h
# End Source File
# Begin Source File

SOURCE=.\Macro.h
# End Source File
# Begin Source File

SOURCE=.\Parser.h
# End Source File
# Begin Source File

SOURCE=.\QTGraphic.h
# End Source File
# Begin Source File

SOURCE=.\resource.h
# End Source File
# Begin Source File

SOURCE=.\StdAfx.h
# End Source File
# Begin Source File

SOURCE=.\SysInfo.h
# End Source File
# Begin Source File

SOURCE=.\Variable.h
# End Source File
# Begin Source File

SOURCE=.\Video.h
# End Source File
# Begin Source File

SOURCE=.\View.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\backup_c.cur
# End Source File
# Begin Source File

SOURCE=.\cursor1.cur
# End Source File
# Begin Source File

SOURCE=.\cursor2.cur
# End Source File
# Begin Source File

SOURCE=.\down_cur.cur
# End Source File
# Begin Source File

SOURCE=.\FiveL.ico
# End Source File
# Begin Source File

SOURCE=.\hand.cur
# End Source File
# Begin Source File

SOURCE=.\left_cur.cur
# End Source File
# Begin Source File

SOURCE=.\Magnify.cur
# End Source File
# Begin Source File

SOURCE=.\right_cu.cur
# End Source File
# Begin Source File

SOURCE=.\small.ico
# End Source File
# Begin Source File

SOURCE=.\up_curso.cur
# End Source File
# End Group
# Begin Source File

SOURCE=.\ReadMe.txt
# End Source File
# End Target
# End Project
