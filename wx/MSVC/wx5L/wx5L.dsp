# Microsoft Developer Studio Project File - Name="wx5L" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=wx5L - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wx5L.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wx5L.mak" CFG="wx5L - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wx5L - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "wx5L - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wx5L - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GR /GX /Zi /O2 /Ob2 /I "../../../Common" /I "../../../libs/freetype2/include" /I "../../../libs/boost" /I "../../../libs/wxWindows/include" /I "../../../libs/wxWindows/contrib/include" /I "../../../libs/wxWindows/lib/msw" /I "../../../libs/quake2/wx" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D WINVER=0x400 /D "_MT" /D wxUSE_GUI=1 /FD /c
# SUBTRACT CPP /YX
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /i "..\..\..\libs\wxWindows\include" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib wsock32.lib qtmlclient.lib ../../../libs/wxWindows/lib/zlib.lib ../../../libs/wxWindows/lib/regex.lib ../../../libs/wxWindows/lib/png.lib ../../../libs/wxWindows/lib/jpeg.lib ../../../libs/wxWindows/lib/tiff.lib ../../../libs/wxWindows/lib/wxmsw.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /out:"../../../Win32/Bin/Tamale.exe" /pdbtype:sept
# SUBTRACT LINK32 /pdb:none /incremental:yes /map /nodefaultlib

!ELSEIF  "$(CFG)" == "wx5L - Win32 Debug"

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
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GR /GX /ZI /Od /I "../../../Common" /I "../../../libs/freetype2/include" /I "../../../libs/boost" /I "../../../libs/wxWindows/include" /I "../../../libs/wxWindows/contrib/include" /I "../../../libs/wxWindows/lib/mswd" /I "../../../libs/quake2/wx" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D WINVER=0x400 /D "_MT" /D wxUSE_GUI=1 /D "__WXDEBUG__" /D WXDEBUG=1 /D "__WXWINDOWS__" /D "REF_HARD_LINKED" /D "IML_Q2_EXTENSIONS" /FD /GZ /c
# SUBTRACT CPP /X /YX
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /i "..\..\..\libs\wxWindows\include" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib qtmlclient.lib ../../../libs/wxWindows/lib/zlibd.lib ../../../libs/wxWindows/lib\regexd.lib ../../../libs/wxWindows/lib\pngd.lib ../../../libs/wxWindows/lib\jpegd.lib ../../../libs/wxWindows/lib\tiffd.lib ../../../libs/wxWindows/lib\wxmswd.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libcmt" /out:"../../../Win32/Bin/Tamale_d.exe" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "wx5L - Win32 Release"
# Name "wx5L - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\src\CursorManager.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Element.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\EventDispatcher.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\FiveLApp.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\HistoryText.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\ImageCache.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Listener.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\LocationBox.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Log5L.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\MovieElement.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\MovieWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\MovieWindowQT.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\dlg\ProgramPropDlg.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\ProgramTree.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\dlg\PropertyDlg.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Quake2Engine.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\resources.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Stage.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\dlg\StartupDlg.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Timecoder.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\ToolWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\TQTMovie.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\TQuake2Primitives.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\TWxPrimitives.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Widget.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\dlg\XrcDlg.cpp
# End Source File
# Begin Source File

SOURCE=..\..\src\Zone.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\src\CursorManager.h
# End Source File
# Begin Source File

SOURCE=..\..\src\Element.h
# End Source File
# Begin Source File

SOURCE=..\..\src\EventDispatcher.h
# End Source File
# Begin Source File

SOURCE=..\..\src\FiveLApp.h
# End Source File
# Begin Source File

SOURCE=..\..\src\HistoryText.h
# End Source File
# Begin Source File

SOURCE=..\..\src\ImageCache.h
# End Source File
# Begin Source File

SOURCE=..\..\src\Listener.h
# End Source File
# Begin Source File

SOURCE=..\..\src\LocationBox.h
# End Source File
# Begin Source File

SOURCE=..\..\src\Log5L.h
# End Source File
# Begin Source File

SOURCE=..\..\src\MovieElement.h
# End Source File
# Begin Source File

SOURCE=..\..\src\MovieWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\src\MovieWindowQT.h
# End Source File
# Begin Source File

SOURCE=..\..\src\dlg\ProgramPropDlg.h
# End Source File
# Begin Source File

SOURCE=..\..\src\ProgramTree.h
# End Source File
# Begin Source File

SOURCE=..\..\src\dlg\PropertyDlg.h
# End Source File
# Begin Source File

SOURCE=..\..\src\Quake2Engine.h
# End Source File
# Begin Source File

SOURCE=..\..\src\Stage.h
# End Source File
# Begin Source File

SOURCE=..\..\src\dlg\StartupDlg.h
# End Source File
# Begin Source File

SOURCE=..\..\src\Timecoder.h
# End Source File
# Begin Source File

SOURCE=..\..\src\ToolWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\src\TQTMovie.h
# End Source File
# Begin Source File

SOURCE=..\..\src\TQuake2Primitives.h
# End Source File
# Begin Source File

SOURCE=..\..\src\TWxPrimitives.h
# End Source File
# Begin Source File

SOURCE=..\..\src\Widget.h
# End Source File
# Begin Source File

SOURCE=..\..\src\dlg\XrcDlg.h
# End Source File
# Begin Source File

SOURCE=..\..\src\Zone.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=..\..\src\backup_c.cur
# End Source File
# Begin Source File

SOURCE=..\..\src\ic_5L.ico
# End Source File
# Begin Source File

SOURCE=..\..\src\ic_card.ico
# End Source File
# Begin Source File

SOURCE=..\..\src\ic_document.ico
# End Source File
# Begin Source File

SOURCE=..\..\src\ic_folder_closed.ico
# End Source File
# Begin Source File

SOURCE=..\..\src\ic_folder_open.ico
# End Source File
# Begin Source File

SOURCE=..\..\src\ic_listener.ico
# End Source File
# Begin Source File

SOURCE=..\..\src\ic_timecoder.ico
# End Source File
# Begin Source File

SOURCE=..\..\src\left_cur.cur
# End Source File
# Begin Source File

SOURCE=..\..\src\right_cu.cur
# End Source File
# Begin Source File

SOURCE=..\..\src\tamale.xrc

!IF  "$(CFG)" == "wx5L - Win32 Release"

!ELSEIF  "$(CFG)" == "wx5L - Win32 Debug"

# Begin Custom Build
InputDir=\src\5L\wx\src
InputPath=..\..\src\tamale.xrc
InputName=tamale

"$(InputDir)\resources.cpp" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cd $(InputDir) 
	..\..\libs\wxWindows\contrib\utils\wxrc\Debug\wxrc.exe /c /o resources.cpp $(InputName).xrc 
	
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\src\tb_borders.bmp
# End Source File
# Begin Source File

SOURCE=..\..\src\tb_grid.bmp
# End Source File
# Begin Source File

SOURCE=..\..\src\tb_reload.bmp
# End Source File
# Begin Source File

SOURCE=..\..\src\tb_xy.bmp
# End Source File
# Begin Source File

SOURCE=..\..\src\up_curso.cur
# End Source File
# Begin Source File

SOURCE=..\..\src\wx5L.rc
# End Source File
# End Group
# Begin Group "Optimized Source Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\src\StageOpt.cpp

!IF  "$(CFG)" == "wx5L - Win32 Release"

!ELSEIF  "$(CFG)" == "wx5L - Win32 Debug"

# PROP Ignore_Default_Tool 1
# Begin Custom Build
OutDir=.\Debug
InputPath=..\..\src\StageOpt.cpp
InputName=StageOpt

"$(OutDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	rem  Use C++ command line, adding: 
	rem    /Zi /O2 /Ob2 $(InputPath) 
	rem  ...and removing: 
	rem    /Zl /Od /GZ 
	cl /nologo /MTd /W3 /Gm /GR /GX /I "../../../Common" /I "../../../libs/freetype2/include" /I "../../../libs/boost" /I "../../../libs/wxWindows/include" /I "../../../libs/wxWindows/contrib/include" /I "../../../libs/wxWindows/lib/mswd" /I "../../../libs/quake2/wx" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D WINVER=0x400 /D "_MT" /D wxUSE_GUI=1 /D "__WXDEBUG__" /D WXDEBUG=1 /D "__WXWINDOWS__" /D "REF_HARD_LINKED" /D "IML_Q2_EXTENSIONS" /Fo"Debug/" /Fd"Debug/" /FD /c /Zi /O2 /Ob2 $(InputPath) 
	
# End Custom Build

!ENDIF 

# End Source File
# End Group
# End Target
# End Project
