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
// TLogger.cpp : 
//

#include <time.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#include "THeader.h"
#include "TCommon.h"
#include "TLogger.h"
#include "TString.h"
#include "TVersion.h"

USING_NAMESPACE_FIVEL

#define FATAL_HEADER	"Fatal Error: "
#define ERROR_HEADER	"Error: "
#define CAUTION_HEADER	"Caution: "

#define FormatMsg(Format)	\
	va_list	argPtr;			\
	va_start(argPtr, (Format));		\
	vsprintf(m_LogBuffer, (Format), argPtr);	\
	va_end(argPtr)

// Standard logs.
TLogger FIVEL_NS gLog;
TLogger FIVEL_NS gDebugLog;
TLogger FIVEL_NS gMissingMediaLog;

TLogger::TLogger()
{ 
	m_LogOpen = false; 
	m_OpenFailed = false;
	m_CautionAlert = false; 
	m_Append = false;
	m_LogMask = LOG_ALL;
}

TLogger::~TLogger()
{
	if (m_LogOpen)
		m_Log.close();
}

//
//	Init - Initialize the log file. 
//
void TLogger::Init(const FileSystem::Path &inLogFile, 
				   bool OpenFile /* = true */, bool Append /* = false */)
{
	ASSERT(not m_LogOpen);

	m_Append = Append;
	m_FileName =
		inLogFile.ReplaceExtension("log").ToNativePathString().c_str();

	if (OpenFile)
	{
		if (m_Append)
			m_Log.open(m_FileName.GetString(), std::ios::out | std::ios::app);
		else
			m_Log.open(m_FileName.GetString(), std::ios::out);

		if (not m_Log.fail())
			m_LogOpen = true;
		else
			m_OpenFailed = true;

		if (m_LogOpen and m_Append)
		{
			// put welcome line
			m_Log << std::endl;
			TimeStamp();
		}
	}
}

//
//	Init - Initialize the log file. Use the current directory.
//
void TLogger::Init(const char *Name, bool OpenFile /* = true */,
				   bool Append /* = false */)
{	
	Init(FileSystem::GetBaseDirectory().AddComponent(Name), OpenFile, Append);
}

void TLogger::Log(int32 Mask, const char *Format, ...)
{
	if (!m_LogOpen)
		return;

	if (not ShouldLog(Mask))
		return;

	FormatMsg(Format);
	LogBuffer(NULL);	
}

void TLogger::Log(const char *Format, ...)
{
	if (!m_LogOpen)
		return;

	FormatMsg(Format);
	LogBuffer(NULL);
}

void TLogger::Error(const char *Format, ...)
{
	if (!m_LogOpen)
		return;

	FormatMsg(Format);
	LogBuffer(ERROR_HEADER);
	AlertBuffer(true);
}

void TLogger::Caution(const char *Format, ...)
{
	if (!m_LogOpen)
		return;

	FormatMsg(Format);
	LogBuffer(CAUTION_HEADER);
	if (m_CautionAlert)
		AlertBuffer(false);
}

void TLogger::FatalError(const char *Format, ...)
{
	if (!m_LogOpen)
		return;

	FormatMsg(Format);
	LogBuffer(FATAL_HEADER);
	AlertBuffer(true);
	exit(1);
}

//
//	CheckLog - Check that the log is open, if it isn't and 
//		we haven't failed at opening it once then try to open
//		it again.
//
/*
bool TLogger::CheckLog()
{
	if (m_LogOpen)
		return (true);

	if (m_OpenFailed)
		return (false);

	if (m_FileName.IsEmpty())
		return (false);

	// else try and open it
	if (m_Append)
		m_Log.open(m_FileName.GetString(), std::ios::out | std::ios::app);
	else
		m_Log.open(m_FileName.GetString(), std::ios::out);
	if (m_Log.fail())
		m_OpenFailed = true;
	else
		m_LogOpen = true;

	if (m_LogOpen and m_Append)
	{
		// put welcome line
		m_Log << std::endl;
		TimeStamp();
	}

	return (m_LogOpen);
}
*/

//
//	LogBuffer - 
//
void TLogger::LogBuffer(const char *Header)
{
	if (m_LogOpen)
	{
		if (Header != NULL)
			m_Log << Header;

		m_Log << m_LogBuffer << std::endl;
	}
}

//
//	AlertBuffer -
//
#ifdef FIVEL_PLATFORM_WIN32

void TLogger::AlertBuffer(bool isError /* = false */)
{
	uint32		alertType;

	alertType = MB_SYSTEMMODAL | MB_OK;
	if (isError)
		alertType |= MB_ICONSTOP;
	else
		alertType |= MB_ICONINFORMATION;

	::MessageBox(::GetFocus(), m_LogBuffer, NULL, alertType);
}

#elif FIVEL_PLATFORM_MACINTOSH

#include <Dialogs.h>
#include <TextUtils.h>

void TLogger::AlertBuffer(bool isError /* = false */)
{
	c2pstr(m_LogBuffer);
	
	::ParamText((const uint8 *) m_LogBuffer, NULL, NULL, NULL);
	if (isError)
    	(void) ::StopAlert(2001, nil);
    else
    	(void) ::CautionAlert(2001, nil);
    
    p2cstr((unsigned char *) m_LogBuffer);
}

#elif FIVEL_PLATFORM_OTHER

void TLogger::AlertBuffer(bool isError /* = false */)
{
	std::cerr << std::endl;
	if (isError)
		std::cerr << "ERROR: ";
	else
		std::cerr << "INFO: ";
	std::cerr << m_LogBuffer << std::endl;
}

#endif


//
//	TimeStamp - Put a time stamp in the log
//
void TLogger::TimeStamp(void)
{
	time_t	timeNow;
	char	*timeStrPtr;

	time(&timeNow);
	timeStrPtr = ctime(&timeNow);
	m_Log << timeStrPtr << std::endl;
}

void TLogger::OpenStandardLogs(bool inShouldOpenDebugLog /*= false*/)
{
	// Initialize the global log file.
	gLog.Init(SHORT_NAME, true, true);
	gLog.Log("%s", VERSION_STRING);

	// Initialize the missing media file.
	gMissingMediaLog.Init("MissingMedia", false, true);

	if (inShouldOpenDebugLog)
	{
		// Initialize the debug log.
		gDebugLog.Init("Debug");
		gDebugLog.Log("%s", VERSION_STRING);
	}	
}


/*
 $Log$
 Revision 1.3.4.2  2002/04/22 08:17:57  emk
 Updated Common code to build on Macintosh and pass all unit tests.

 Revision 1.3.4.1  2002/04/19 11:20:13  emk
 Start of the heavy typography merging work.  I'm doing this on a branch
 so I don't cause problems for any of the other developers.

 Alpha-blend text colors.

 Merged Mac and Windows versions of several files into the Common directory.
 Not all of these work on Mac and/or Windows yet, but they're getting there.
 Primary sources for the merged code are:

   Win/FiveL/LVersion.h -> Common/TVersion.h
   Win/FiveL/LStream.h -> Common/TStream.h
   Mac/Source/CStream.cp -> Common/TStream.cpp
   Mac/Source/CStreamTests.cp -> Common/TStreamTests.cpp

 TStream changes:

   * The TStream code now uses a callback to variable values.  This will
     probably go away once Variable and CVariable get merged.
   * Input operators for std::string and GraphicTools::Color.

 Isolated Windows-specific code in TLogger.*, in preparation for a big merge.

   * Added a portable function to set up logging.
   * Fixed the logging code to use the portable FileSystem library.
   * Made FatalError actually quit the application.

 Turned off the FiveL namespace on FIVEL_PLATFORM_OTHER, so we can debug
 with GDB, which has a few minor but painful namespace issues.

 TString changes:

   * Made sure we can convert from std::string to a TString.
   * Added some more assertions.
   * Fixed bug in various operator= methods which would allow the string's
     internal data pointer to be NULL.
   * Changed operator[] and operator() arguments to be 'int' instead of
     'int32' to avoid nasty compiler warnings.

 Typography::Style changes:

   * Added a "ShadowOffset" field that specifies the offset of the
     drop shadow.
   * Added an operator== for testing.
   * Added a ToggleFaceStyle method for toggling specified face style bits.

 Typography::StyledText changes:

   * Added a method to append a single character.

 Other Typography changes:

   * Made FaceStyle an int, not an enum, so we can do bit math with it.
   * Added assertions to made sure you can't extract a StyledText iterator
     until you've called EndConstruction.

 Revision 1.3  2002/02/19 12:35:11  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.2  2002/01/23 20:39:19  tvw
 A group of changes to support a new stable build.

 (1) Only a single instance of the FiveL executable may run.

 (2) New command-line option "-D" used to lookup the installation directory in the system registry.
     Note: Underscores will be parsed as spaces(" ").
     Ex: FiveL -D HIV_Prevention_Counseling

 (3) Slow down the flash on buttpcx so it can be seen on
     fast machines.  A 200 mS pause was added.

 (4) Several bugfixes to prevent possible crashes when error
     conditions occur.

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.1  2000/04/06 17:06:10  chuck
 Initial check-in

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
