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

#if !defined (_SysInfo_h_)
#define _SysInfo_h_

#include "TString.h"

/*-----------------------------------------------------------------

CLASS
    SysInfo

	A class that pulls Operating System information.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class SysInfo : public TObject
{
public:
	
	//////////
	// Initialize.  Figures out OS information.
	//
	Init(void);
	
	//////////
	// Are we running on a Win32 operating system?
	//
	// [out] return - true if the system is a Win32 OS (and not NT 3.51), false otherwise
	//
	bool		IsWin32(void);
	
	//////////
	// Return a string with OS info.
	//
	// [out] return - an string with OS information
	//
	TString		SysString(void);
	
	//////////
	// Return a shortened string with OS info.
	//
	// [out] return - a short string with OS info
	//
	TString		ShortString(void) { return (m_ShortString); }

private:
	//////////
	// OS Version Info
	//
	OSVERSIONINFO		m_Info;
	
	//////////
	// Short version string.
	//
	TString				m_ShortString;
};


#endif // _SysInfo_h_

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
