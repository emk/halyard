# Microsoft Developer Studio Project File - Name="Common" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=Common - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Common.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Common.mak" CFG="Common - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Common - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Common - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Common - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
LINK32=link.exe
MTL=midl.exe
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GR /GX /O2 /I "..\..\..\Common" /I "..\..\..\libs\libxml2\include" /I "..\..\..\libs\freetype2\include" /I "..\..\..\libs\plt\include" /I "..\..\..\libs\crypto" /I "..\..\..\libs\boost" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /D "LIBXML_STATIC" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "Common - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
LINK32=link.exe
MTL=midl.exe
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GR /GX /ZI /Od /I "..\..\..\Common" /I "..\..\..\libs\libxml2\include" /I "..\..\..\libs\freetype2\include" /I "..\..\..\libs\plt\include" /I "..\..\..\libs\crypto" /I "..\..\..\libs\boost" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "LIBXML_STATIC" /FR /Yu"CommonHeaders.h" /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "Common - Win32 Release"
# Name "Common - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\..\Common\doc\Background.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\CommonHeaders.cpp

!IF  "$(CFG)" == "Common - Win32 Release"

!ELSEIF  "$(CFG)" == "Common - Win32 Debug"

# ADD CPP /Yc"CommonHeaders.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\Common\doc\Document.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\FileSystem.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\Model.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\ModelChange.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\ModelView.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\doc\TamaleProgram.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TArray.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TBTree.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TCommonPrimitives.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TDateUtil.cpp

!IF  "$(CFG)" == "Common - Win32 Release"

!ELSEIF  "$(CFG)" == "Common - Win32 Debug"

# ADD CPP /Yu

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TDeveloperPrefs.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TEncoding.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TestCase.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TException.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TInterpreter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TLogger.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TObject.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TPoint.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TPolygon.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TPrimitives.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TRect.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\lang\scheme\TSchemeCallback.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\lang\scheme\TSchemeInterpreter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TStartup.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TString.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TStyleSheet.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TTemplateUtils.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TURL.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TValue.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TVariable.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\Typography.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\XmlUtils.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\..\Common\doc\Background.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\CommonHeaders.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\doc\Document.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\FileSystem.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\GraphicsTools.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\Model.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\ModelChange.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\ModelView.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\doc\TamaleProgram.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TArray.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TBTree.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TCommon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TCommonPrimitives.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TDateUtil.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TDeveloperPrefs.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TEncoding.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TestCase.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TException.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\THeader.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TInterpreter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TLogger.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TObject.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TPlatform.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TPoint.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TPolygon.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TPrimitives.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TRect.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\lang\scheme\TSchemeCallback.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\lang\scheme\TSchemeInterpreter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\lang\scheme\TSchemePtr.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TStartup.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TString.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TStyleSheet.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TTemplateUtils.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TURL.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TUtilities.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TValue.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TVariable.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TVectorDiff.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\TVersion.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\Typography.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Common\XmlUtils.h
# End Source File
# End Group
# Begin Group "Optimized Source Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\Common\GraphicsTools.cpp

!IF  "$(CFG)" == "Common - Win32 Release"

!ELSEIF  "$(CFG)" == "Common - Win32 Debug"

# PROP Ignore_Default_Tool 1
USERDEP__GRAPH="../../../Common/CommonHeaders.h"	"../../../Common/GraphicsTools.h"	
# Begin Custom Build
OutDir=.\Debug
InputPath=..\..\..\Common\GraphicsTools.cpp
InputName=GraphicsTools

"$(OutDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	rem  Use C++ command line, adding: 
	rem    /Zi /O2 /Ob2 $(InputPath) 
	rem  ...and removing: 
	rem    /Zl /Od /GZ 
	cl /nologo /MTd /W3 /Gm /GR /GX /I "..\..\..\Common" /I "..\..\..\libs\libxml2\include" /I "..\..\..\libs\freetype2\include" /I "..\..\..\libs\plt\include" /I "..\..\..\libs\crypto" /I "..\..\..\libs\boost" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "LIBXML_STATIC" /FR"Debug/" /Fo"Debug/" /Fd"Debug/" /FD /c /Zi /O2 /Ob2 $(InputPath) 
	
# End Custom Build

!ENDIF 

# End Source File
# End Group
# End Target
# End Project
