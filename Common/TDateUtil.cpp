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

// forward declarations
static void GetTime(char *inStr, struct tm *inTime);
static void GetShortDate(char *inStr, struct tm *inTime);
static void GetLongDate(char *inStr, struct tm *inTime);
static void GetShortYear(char *inStr, struct tm *inTime);
static void GetYear(char *inStr, struct tm *inTime);
static void GetMonth(char *inStr, int inMonth);
static void GetDay(char *inStr, int inDay);


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
// Date and time functions.
// 

//
//	GetDate - Get the current date or time.
//
void GetDate(TString &inStr, const DateFormat format)
{
	char		theResult[128];
	struct tm	*theLTime;
	time_t		theTime;
	
	time(&theTime);
    theLTime = localtime(&theTime);      			/* Convert to local time. */
    
    theResult[0] = 0;
    
    switch (format)
    {
    	case df_TIME:
    		GetTime(theResult, theLTime);
    		break;
    	case df_LONGDATE:
    		GetLongDate(theResult, theLTime);
    		break;
    	case df_DATE:
    		GetShortDate(theResult, theLTime);
    		break;
    	case df_SECONDS:
    		wsprintf(theResult, "%ld", theTime);
    		break;
    }
	
	inStr = (const char *) theResult;
}

//
//	SetDate - Set inStr based on the passed in time (inSecs) and
//		the inDateType parameter.
//
void SetDate(TString &inStr, int32 inSecs, int32 inDateType)
{
	char		theResult[128];
	time_t		theTime = inSecs;
	struct tm	*theLTime;
	
	theLTime = localtime(&theTime);
	
	theResult[0] = 0;
	
	switch (inDateType)
	{
		case DT_TIME:
			GetTime(theResult, theLTime);
			break;
		case DT_LONGDATE:
			GetLongDate(theResult, theLTime);
			break;
		case DT_DATE:
			GetShortDate(theResult, theLTime);
			break;
		case DT_YEAR:
			GetYear(theResult, theLTime);
			break;
		case DT_MONTH:
			sprintf(theResult, "%d", theLTime->tm_mon + 1);
			break;
		case DT_LONGMONTH:
			GetMonth(theResult, theLTime->tm_mon + 1);
			break;
		case DT_DAY:
			sprintf(theResult, "%d", theLTime->tm_mday);
			break;
		case DT_LONGDAY:
			GetDay(theResult, theLTime->tm_wday);
			break;	
	}
	
	inStr = (const char *) theResult;
}

static void GetTime(char *inStr, struct tm *inTime)
{ 
	char	am_pm[] = "AM";
	char	min[3];
	
	if ((inTime != NULL) and (inStr != NULL))
	{			
		if (inTime->tm_hour > 12)
		{
			strcpy(am_pm, "PM");
			inTime->tm_hour -= 12;
		}
		if (inTime->tm_hour == 0)
			inTime->tm_hour = 12;
		if (inTime->tm_min > 9)
			wsprintf(min, "%d", inTime->tm_min);
		else
			wsprintf(min, "0%d", inTime->tm_min);
			
		wsprintf(inStr, "%d:%s %s", inTime->tm_hour, min, am_pm);
	}
} 

static void GetShortDate(char *inStr, struct tm *inTime)
{
	char	theYear[5];
	if ((inStr != NULL) and (inTime != NULL))
	{
		GetShortYear(theYear, inTime);
		wsprintf(inStr, "%d/%d/%s", inTime->tm_mon + 1,
			inTime->tm_mday, theYear);
	}
}

static void GetLongDate(char *inStr, struct tm *inTime)
{
	char	theYear[5];
	char	theMonth[20];
	char	theDay[15];
	
	if ((inStr != NULL) and (inTime != NULL))
	{
		GetYear(theYear, inTime);
		GetMonth(theMonth, inTime->tm_mon + 1);
		GetDay(theDay, inTime->tm_wday);
	
		wsprintf(inStr, "%s, %s %d, %s", theDay, theMonth, 
			inTime->tm_mday, theYear);
	}
}	

static void GetShortYear(char *inStr, struct tm *inTime)
{
	int		theYear;

	if ((inTime != NULL) and (inStr != NULL))
	{
		// 2000 comes through as 100
		if (inTime->tm_year >= 100)
			theYear = inTime->tm_year - 100;
		else
			theYear = inTime->tm_year;
		
		wsprintf(inStr, "%02d", theYear);
	}
}

static void GetYear(char *inStr, struct tm *inTime)
{
	int		theYear;
	
	if ((inTime != NULL) and (inStr != NULL))
	{
		// 2000 comes through as 100
		theYear = 1900 + inTime->tm_year;
		
		wsprintf(inStr, "%d", theYear);
	}
}

static void GetMonth(char *inStr, int inMonth)
{
	if ((inStr != NULL) and (inMonth > 0) and (inMonth <= 12))
	{
		switch (inMonth)
		{
			case 1:
				strcpy(inStr, "January");
				break;    
			case 2:
				strcpy(inStr, "February");
				break;    
			case 3:
				strcpy(inStr, "March");
				break;    
			case 4:
				strcpy(inStr, "April");
				break;    
			case 5:
				strcpy(inStr, "May");
				break;    
			case 6:
				strcpy(inStr, "June");
				break;    
			case 7:
				strcpy(inStr, "July");
				break;    
			case 8:
				strcpy(inStr, "August");
				break;    
			case 9:
				strcpy(inStr, "September");
				break;    
			case 10:
				strcpy(inStr, "October");
				break;    
			case 11:
				strcpy(inStr, "November");
				break;    
			case 12:
				strcpy(inStr, "December");
				break;    
		}
	}
}

static void GetDay(char *inStr, int inDay)
{
	if ((inStr != NULL) and (inDay >= 0) and (inDay < 7))
	{
		switch (inDay)
		{
			case 0:
				strcpy(inStr, "Sunday");
				break;
			case 1:
				strcpy(inStr, "Monday");
				break;
			case 2:
				strcpy(inStr, "Tuesday");
				break;
			case 3:
				strcpy(inStr, "Wednesday");
				break;
			case 4:
				strcpy(inStr, "Thursday");
				break;
			case 5:
				strcpy(inStr, "Friday");
				break;
			case 6:
				strcpy(inStr, "Saturday");
				break;
		}
	}
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
 Revision 1.3.2.1  2002/03/13 15:06:56  emk
 Merged changed from 3.1.1 -> 3.2.1 into the 3.2.0.1 codebase,
 because we want these in the stable engine.  Highlights:

   1) FiveL.prefs file support.
   2) Removal of -D command line flag.

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
