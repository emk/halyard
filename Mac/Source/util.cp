/* util.c

    This file contains an assortment of routines mostly dealing
    with initialization and input commands.

*/

#include "debug.h"

#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <iostream.h>

#include "Mac5L.h"
#include "CMac5LApp.h"
#include "CConfig.h"
#include "CString.h"
#include "CPlayerView.h"
#include "util.h"

#include "gamma.h"


// globals
extern bool			gPrintToFile;

#ifdef DEBUG_5L
//#define FILE_CREATOR	'R*ch'
const long 			FILE_CREATOR = FOUR_CHAR_CODE('R*ch');
const char			*LogFileName = "debug.log";
#else
const long 			FILE_CREATOR = FOUR_CHAR_CODE('ttxt');
//#define FILE_CREATOR	'ttxt'
const char 			*LogFileName = "Log";
#endif
//#define FILE_TYPE		'TEXT'
const long			FILE_TYPE = FOUR_CHAR_CODE('TEXT');

CTextFileStream		*gDebugFile = nil;
const int 			PRINFO_BUF_SIZE = 50000;
static char			*prinfo_buf = NULL;

//
//	PinRange - Return a number that is between the given min and max.
//
int16 PinRange(int16 num, int16 minimum, int16 maximum)
{
    if (num < minimum) 
    	return minimum;
    else if (num > maximum) 
    	return maximum;
    else 
    	return num;
}

//
//	prerror - Puts up an alert and stops the program. Takes arguments like
//				printf for the string to display.
//
void prerror(char *cs,...)
{
    va_list 	arg_ptr;
    char 		buf[255];
    
    DoGFade(true, 0);			// make sure we aren't faded out
    
    va_start(arg_ptr, cs);
    
    vsprintf(buf, cs, arg_ptr);
    
    c2pstr(buf);
    
    ::ParamText((const uint8 *) buf, NULL, NULL, NULL);
    (void) ::StopAlert(2001, nil);
	
	va_end(arg_ptr);
	
	gTheApp->DoQuit();
}

//
//	prcaution - Exactly like prinfo for now.
//
void prcaution(char *cs,...)
{
	va_list		arg_ptr;

	if (gPrintToFile)
	{	
		va_start(arg_ptr, cs);
		
		if (prinfo_buf == NULL)
			prinfo_buf = ::NewPtr(PRINFO_BUF_SIZE);
			
		if (prinfo_buf == NULL)
			return;
		
		vsprintf(prinfo_buf, cs, arg_ptr);
		
		if (gDebugFile != nil)
		{
			Int32	strLen;
			char	endLine[2];

			endLine[0] = NEWLINE_CHAR;
			endLine[1] = '\0';
						
			strcat(prinfo_buf, endLine);
			strLen = strlen(prinfo_buf);
			
			gDebugFile->Write(prinfo_buf, strLen);
			
			// don't flush
			// gDebugFile->Flush();
		}
		
		va_end(arg_ptr);
	}
}

//
//	prinfo - Print out an info string. It can be printed to a SIOUX window or to a file
//			depending on the settings of the gPrint and gPrintToFile globals.
//
void prinfo(char *cs,...)
{
	va_list		arg_ptr;

	if (gPrintToFile)
	{	
		va_start(arg_ptr, cs);
		
		if (prinfo_buf == NULL)
			prinfo_buf = ::NewPtr(PRINFO_BUF_SIZE);
			
		if (prinfo_buf == NULL)
			return;
		
		vsprintf(prinfo_buf, cs, arg_ptr);
		
		if (gDebugFile != nil)
		{
			Int32	strLen;
			char	endLine[2];

			endLine[0] = NEWLINE_CHAR;
			endLine[1] = '\0';
						
			strcat(prinfo_buf, endLine);
			strLen = strlen(prinfo_buf);
			
			gDebugFile->Write(prinfo_buf, strLen);
			
			// don't flush
			// gDebugFile->Flush();
		}
		
		va_end(arg_ptr);
	}
}

//
//	prmem - Print out the amount of free memory. Use prinfo which will look at the globals.
//
void prmem(void)
{
	int32	freeMem;
	
	freeMem = ::FreeMem();
	prinfo("Free Memory: %d", freeMem);
}

//
//	pronecommand - Print out the next command to be executed. This assumes that commands
//					fit on one line and there is only one per line.
//
void pronecommand(char *inStr, uint32 inPos)
{
	char	theBuf[128];
	char	*thePtr;
	int32	theCount = 0;
	
	if (inStr != NULL)
	{
		thePtr = &(inStr[inPos]);
		while ((*thePtr != NEWLINE_CHAR) and (*thePtr != RETURN_CHAR))
		{
			theBuf[theCount++] = *thePtr++;
		}
		theBuf[theCount] = '\0';
	
		prinfo("<next command>: %s", theBuf);
	}
}

//
//	open_debug_file - Open up the dump file for debugging.
//
void open_debug_file(void)
{
	FSSpec	debugSpec;
	FInfo	theFInfo;
	OSErr	err;

	try
	{
		gDebugFile = nil;
			
		theConfig->FillDebugSpec(&debugSpec, LogFileName);
		
		gDebugFile = new CTextFileStream(debugSpec);
		
		err = FSpGetFInfo(&debugSpec, &theFInfo);
		
		if (err == fnfErr)
			gDebugFile->CreateNewDataFile(FILE_CREATOR, FILE_TYPE);
			
		gDebugFile->OpenDataFork(fsRdWrPerm);
		gDebugFile->SetLength(0);
	}
	
	catch (const LException& inException)
	{
		if (gDebugFile != nil)
			close_debug_file();
			
		gPrintToFile = false;
	}
	
}

//
//	close_debug_file - Close the dump file.
//
void close_debug_file(void)
{
	if (gPrintToFile)
	{
		gDebugFile->CloseDataFork();
		delete gDebugFile;
	}
}	

//
//	trim - Trim trailing spaces from a string.
//
void trim(char *str)
{
    char    *ptr;

    ptr = str;
    while(*ptr)
        ptr++;
    ptr--;
    while(ptr >= str && *ptr == ' ')
        ptr--;
    ++ptr;
    *ptr = '\0';
}

//
//	strlower - Change all upper case characters to lower case in the
//				given string.
//
void strlower(char *str)
{
	if (str != NULL)
	{
		while (*str != '\0')
		{
			if (isupper(*str))
				*str = tolower(*str);
			*str++;
		}
	}
}

//
//	strnicmp - Compare the first count characters of the two strings without 
//				regard to case.
//
int16 strnicmp(const char *str1, const char *str2, int32 count)
{
	unsigned char	ch1, ch2;
	bool			done = FALSE;
	
	if ((str1 != NULL) and (str2 != NULL))
	{
		while ((not done) and (*str1 != '\0') and (*str2 != '\0'))
		{
			ch1 = tolower(*str1++);
			ch2 = tolower(*str2++);
			
			if (ch1 < ch2)
				return (-1);
			else if (ch1 > ch2)
				return (1);
				
			if (--count == 0)
				done = TRUE;
		}
		
		// done comparing, see why we stopped
		if (not done)
		{
			if ((*str1 == '\0'))
			{
				if (*str2 == '\0')
					return (0);		// both at end, strings are equal
				else
					return (-1);	// str1 at end, str2 greater
			}
			else
				return (1);			// must be str2 at end, str1 greater
		}
		else
			return (0);				// at count chars, strings are equal
	}
	
	ASSERT(FALSE);					
	return (0);
}

//
//	stricmp - Compare the two strings without regard to case.
//
int16 stricmp(const char *str1, const char *str2)
{
	unsigned char	ch1, ch2;
	
	if ((str1 != NULL) and (str2 != NULL))
	{
		while ((*str1 != '\0') and (*str2 != '\0'))
		{
			ch1 = tolower(*str1++);
			ch2 = tolower(*str2++);
			
			if (ch1 < ch2)
				return (-1);
			else if (ch1 > ch2)
				return (1);
		}
		
		// done comparing see why we stopped
		if ((*str1 == '\0'))
		{
			if (*str2 == '\0')
				return (0);		// both at end, strings are equal
			else
				return (-1);	// str1 at end, str2 greater
		}
		else
			return (1);			// must be str2 at end, str1 greater
		
	}
	
	ASSERT(FALSE);
	return (0);
}
