// -*- Mode: C++; tab-width: 4; -*-
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

#include <time.h>

#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

#include "TDateUtil.h"

USING_NAMESPACE_FIVEL

// forward declarations
static void GetTime(char *inStr, struct tm *inTime);
static void GetShortDate(char *inStr, struct tm *inTime);
static void GetLongDate(char *inStr, struct tm *inTime);
static void GetShortYear(char *inStr, struct tm *inTime);
static void GetYear(char *inStr, struct tm *inTime);
static void GetMonth(char *inStr, int inMonth);
static void GetDay(char *inStr, int inDay);


//
// Date and time functions.
// 

//
//	GetDate - Get the current date or time.
//
void FIVEL_NS GetDate(TString &inStr, const DateFormat format)
{
	char		theResult[128];
	struct tm	*theLTime;
	time_t		theTime;
	
	time(&theTime);
    theLTime = localtime(&theTime); // Convert to local time.
    
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
    		sprintf(theResult, "%ld", theTime);
    		break;
    }
	
	inStr = (const char *) theResult;
}

//
//	SetDate - Set inStr based on the passed in time (inSecs) and
//		the inDateType parameter.
//
void FIVEL_NS SetDate(TString &inStr, int32 inSecs, int32 inDateType)
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
			sprintf(min, "%d", inTime->tm_min);
		else
			sprintf(min, "0%d", inTime->tm_min);
			
		sprintf(inStr, "%d:%s %s", inTime->tm_hour, min, am_pm);
	}
} 

static void GetShortDate(char *inStr, struct tm *inTime)
{
	char	theYear[5];
	if ((inStr != NULL) and (inTime != NULL))
	{
		GetShortYear(theYear, inTime);
		sprintf(inStr, "%d/%d/%s", inTime->tm_mon + 1,
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
		sprintf(inStr, "%s, %s %d, %s", theDay, theMonth, 
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
		sprintf(inStr, "%02d", theYear);
	}
}

static void GetYear(char *inStr, struct tm *inTime)
{
	int		theYear;
	
	if ((inTime != NULL) and (inStr != NULL))
	{
		// 2000 comes through as 100
		theYear = 1900 + inTime->tm_year;
		sprintf(inStr, "%d", theYear);
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


/*
 $Log$
 Revision 1.4.2.2  2002/04/22 13:20:07  emk
 Major Mac cleanups:

 - We use subprojects to recursively build 5L and its support libraries.  This means no more opening up four different projects, etc., just to do a build.

 - Search & replaced class names for merged classes.  This doesn't quite work or build yet, but I'm working on a branch, so that's OK.

 - Other Mac build fixes.

 Tomorrow: Get things running again, make a test binary, and continue merging.

 Revision 1.4.2.1  2002/04/22 05:22:33  emk
 A weekend's worth of merging, in preparation for the Typography switchover.

 MOVED
 -----

 * Win32/Crypt/md5.c -> Common/libs/crypto/md5.c
 * Win32/Crypt/md5.h -> Common/libs/crypto/md5.h
 * Win32/Crypt/md5main.c -> Common/libs/crypto/md5main.c
 * Win32/Crypt/_blowfish.c -> Common/libs/crypto/blowfish.c
 * Win32/Crypt/blowfish.h -> Common/libs/crypto/blowfish.h

 Third-party cryptography files moved to the new Common/libs/crypto
 directory.  In general, third-party code should go under Common/libs, so we
 can find it all in one place for updates and license checks.
 Common/freetype2 will probably move there soon for the sake of consistency.

 MERGED
 ------

 * Win32/Crypt/CryptStream.cpp -> Common/CryptStream.cpp
 * Win32/Crypt/CryptStream.h -> Common/CryptStream.h
 * Win32/TestSuite/TestCryptStream.cpp -> Common/CryptStreamTests.cpp

 Modified to use the portable Path abstraction.  Included our standard key
 once in this file, instead of having it in many different headers
 throughout the program. Coerced uchar* to char* in several places required
 by the fstream API (and some other coercions).

 * Win32/FiveL/Parser.cpp -> Common/TParser.cpp
 * Win32/FiveL/Parser.h -> Common/TParser.h

 Merged in Elizabeth's improved escape-handling code.  Factored out all code
 which specifically referred to "card", "header" or "macrodef" forms, and
 added a generic API for registering abitrary top-level forms.

 * Win32/FiveL/Index.cpp -> Common/TIndex.cpp
 * Win32/FiveL/Index.h -> Common/TIndex.h
 * NEW: Common/TIndexTests.cpp
 * NEW: Common/Scripts/test.scr

 Merged TIndex::GetScript from the Macintosh.  Temporarily stopped closing
 the TIndexFile in the presence of REDOSCRIPT.  Merged some Macintosh code
 for building indices from FSSpecs; this probably doesn't work.  Changed the
 Open and Init methods to use the portable Path library (the APIs might be
 slightly suboptimal).

 * Win32/FiveL/LUtil.cpp -> Common/TDateUtil.cpp
 * Win32/FiveL/LUtil.h -> Common/TDateUtil.h

 Extracted date-related code from LUtil.*.  Changed wsprintf calls to
 sprintf.

 * Win32/FiveL/Variable.cpp -> Common/TVariable.cpp
 * Win32/FiveL/Variable.h -> Common/TVariable.h

 Disabled certain special variables that caused awkward dependencies, and
 replaced them with an interface for registering arbitrary special
 variables.

 MODIFIED
 --------

 * Common/FileSystem.cpp
 * Common/FileSystem.h

 Added a RenameFile function, and a GetScriptsDirectory function.  Also
 added a ReplaceWithTemporaryFile function, which overwrites an existing
 file with a temporary file (someday, we can implement this as an atomic
 operation on most operating systems).

 * Common/GraphicsTools.h

 Added a no-arguments constuctor for Point.

 * Common/TString.cpp
 * Common/TString.h

 Lots of "signed/unsigned comparison" and other warning fixes.

 * Common/TStyleSheet.cpp
 * Common/TStyleSheet.h

 Added full-fledged INCR_X, INCR_Y support!

 * Common/Typography.cpp
 * Common/Typography.h

 Made sure that kerning+advance can never move the drawing cursor backwards.
 Fixed warnings.

 * Common/fonttools/pngtest.cpp

 Added a test of transparent text (just for fun).

 KNOWN ISSUES
 ------------

 * Logging code needs to have Mac-specific features merged back in.

 * TIndexFile doesn't close the underlying file properly in the presence of
 REDOSCRIPT.  What's going on here?

 * TParser--and maybe TStream--need to have cross-platform end-of-line
 handling.

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
