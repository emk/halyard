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
// LUtil.cpp : This file contains an assortment of routines mostly dealing
//    with initialization and input commands.
//

#include "stdafx.h"

#include <time.h>

#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

#include "LUtil.h"

// Makes a message box to alert the user
void AlertMsg(const char *msg, bool isError)
{
	uint32		alertType;

	alertType = MB_SYSTEMMODAL | MB_OK;
	if (isError)
		alertType |= MB_ICONSTOP;
	else
		alertType |= MB_ICONINFORMATION;

	::MessageBox(::GetFocus(), msg, NULL, alertType);
}

//
//	VolIsMounted - 
//	
int32 VolIsMounted(char *inCDPath, TString &inVolName)
{
	int32	retValue = NO_VOLUME;
	char	theBuf[255];
	DWORD	theBufLen = 255;
	DWORD	theMaxLen;
	DWORD	theSysFlags;
	char	theNameBuf[32];
	DWORD	theNameBufLen = 32;

	if (::GetVolumeInformation(inCDPath, theBuf, theBufLen, 
			NULL, &theMaxLen, &theSysFlags, theNameBuf, theNameBufLen))
	{
		// now compare to see if this is it
		if (inVolName.Compare(theBuf, false))
			retValue = WRONG_VOLUME;
		else
			retValue = OK_VOLUME;
	}
	else
		retValue = NO_VOLUME;

	return(retValue);

/*  WIN16

	struct _find_t 	fileinfo; 
	DWORD			now_time;
	DWORD			start_time;
    int				result;
    
	result = _dos_findfirst(inCDPath, _A_VOLID, &fileinfo);
	fileinfo.name[8] = '\0';
	if (result == 0)
	{
		if (inVolName.Compare(fileinfo.name, false))
		{ 
			gDebugLog.Log("CheckDisc: failed first try, disc inserted is <%s>", fileinfo.name);

			// wait a bit and try again - why is this necessary???
			start_time = ::GetTickCount();
			now_time = start_time;
			while ((now_time - start_time) < 120) 
			{
				now_time = ::GetTickCount();
			}
			
			result = _dos_findfirst(inCDPath, _A_VOLID, &fileinfo);
			fileinfo.name[8] = '\0';
			if (result == 0)	
			{	
				if (inVolName.Compare(fileinfo.name, false))
				{
					gDebugLog.Log("CheckDisc: failed 2nd try, disc inserted is <%s>", fileinfo.name);
					retValue = WRONG_VOLUME;
				}
				else
					retValue = OK_VOLUME;
			}
			else
			{
				gDebugLog.Log("CheckDisc: 2nd _dos_findfirst failed, returned <%d>", result);
				retValue = NO_VOLUME;
			}
		}
		else
			retValue = OK_VOLUME;
	}
	else
	{
		gDebugLog.Log("CheckDisc: first _dos_findfirst failed, returned <%d>", result);
		retValue = NO_VOLUME;
	}
	return(retValue);
*/
}

/*
 $Log$
 Revision 1.5  2002/07/29 22:02:46  zeb
   * zeb: Moved some primitives from TMacPrimitives.cpp and TWinPrimitives.cpp
     to TCommonPrimitives.cpp and T5LPrimitives.cpp.
   * zeb: Added a GET primitive.
   * zeb: Cleaned date stuff out of LUtil.{h,cpp}.

 Revision 1.4  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.3  2002/01/24 19:22:41  tvw
 Fixed bug (#531) in -D command-line option causing
 system registry read error.

 Revision 1.2  2002/01/23 20:39:20  tvw
 A group of changes to support a new stable build.

 (1) Only a single instance of the FiveL executable may run.

 (2) New command-line option "-D" used to lookup the installation directory in the system registry.
     Note: Underscores will be parsed as spaces(" ").
     Ex: FiveL -D HIV_Prevention_Counseling

 (3) Slow down the flash on buttpcx so it can be seen on
     fast machines.  A 200 mS pause was added.

 (4) Several bugfixes to prevent possible crashes when error
     conditions occur.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/08/08 19:03:40  chuck
 no message

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
