//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// SysInfo.cpp : 
//

#include "stdafx.h"

#include "TCommon.h"
#include "SysInfo.h"

SysInfo::Init(void)
{
	m_Info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);

	::GetVersionEx(&m_Info);

	if (m_Info.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
	{
		if (m_Info.dwMinorVersion == 0)
			m_ShortString = "Win95";
		else if (m_Info.dwMinorVersion > 0)
			m_ShortString = "Win98";
		else
			m_ShortString = "Win32";
	}
	else if (m_Info.dwPlatformId == VER_PLATFORM_WIN32_NT)
	{
		if (m_Info.dwMajorVersion == 4)
			m_ShortString = "WinNT4";
		else if (m_Info.dwMajorVersion == 5)
			m_ShortString = "Win2000";
		else 
			m_ShortString = "WinNT";
	}
	else
		m_ShortString = "Win";
}

//
//	IsWin32 - Return true if we are Win32 (and not NT 3.51).
//
bool SysInfo::IsWin32(void)
{
	if ((m_Info.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS) or 
		((m_Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and
		 (m_Info.dwMajorVersion >= 4)))
		return (true);
	else 
		return (false);
}

//
//	SysString - Return a string with the system info.
//
TString SysInfo::SysString(void)
{
	TString		sysStr;

	if (m_Info.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
	{
		if (m_Info.dwMinorVersion == 0)
			sysStr = "Windows 95, Build ";
		else if (m_Info.dwMinorVersion > 0)
			sysStr = "Windows 98, Build ";
		else 
			sysStr = "Unknown Windows";

		// get the build number
		int32	buildNum = LOWORD(m_Info.dwBuildNumber);

		sysStr += buildNum;
	}
	else if (m_Info.dwPlatformId == VER_PLATFORM_WIN32_NT)
	{
		if (m_Info.dwMajorVersion == 4)
			sysStr = "Windows NT 4.0, Build ";
		else if (m_Info.dwMajorVersion == 5)
			sysStr = "Windows 2000, Build ";
		else 
			sysStr = "Unknown Windows NT";

		sysStr += (const long) m_Info.dwBuildNumber;
	}

	sysStr += ", ";
	sysStr += m_Info.szCSDVersion;

	return (sysStr);
}

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  1999/11/02 17:16:37  chuck
 2.00 Build 8

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
