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

void (*TLogger::s_ErrorPrepFunction)() = NULL;

#ifdef FIVEL_PLATFORM_MACINTOSH
bool TLogger::s_ToolboxIsInitialized = false;
#endif

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

	FileSystem::Path file_path = inLogFile.ReplaceExtension("log");
	
	m_Append = Append;
	m_FileName = file_path.ToNativePathString().c_str();

	if (OpenFile)
	{
		if (!file_path.DoesExist())
			file_path.CreateWithMimeType("text/plain");
	
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
	// XXX - This old code is completely wrong.  We definitely
	// shouldn't continue from here!
	if (!m_LogOpen)
		return;

	// We call AlertBuffer before LogBuffer, because
	// the AlertBuffer code is required NOT to
	// call back into FatalError, whereas LogBuffer
	// relies on a lot of subsystems which might
	// somehow fail.
	FormatMsg(Format);
	AlertBuffer(true);
	LogBuffer(FATAL_HEADER);
	abort();
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
#ifdef FIVEL_PLATFORM_MACINTOSH
	// On the Macintosh, the log buffer will contain a mix of \r
	// line endings (read in from scripts) with \n line endings
	// (generated by C++ code).  Try to patch this up a bit
	// before writing it.
	// This code merged from the old KLogger class.
	int	logLen = strlen(m_LogBuffer);
	for (int i = 0; i < logLen; i++)
	{
		if (m_LogBuffer[i] == '\r')
			m_LogBuffer[i] = '\n';
	}
#endif // FIVEL_PLATFORM_*

	if (m_LogOpen)
	{
		if (Header != NULL)
			m_Log << Header;

		m_Log << m_LogBuffer << std::endl;
	}
}

//
//	AlertBuffer - Display an alert.
//
//  THIS ROUTINE MAY NOT USE 'ASSERT' OR 'FatalError', BECAUSE IS CALLED
//  BY THE ERROR-LOGGING CODE!
//
#ifdef FIVEL_PLATFORM_WIN32

void TLogger::AlertBuffer(bool isError /* = false */)
{
	PrepareToDisplayError();

	uint32 alertType = MB_SYSTEMMODAL | MB_OK;
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
	PrepareToDisplayError();

	// Make sure we don't call StopAlert or CautionAlert before
	// the Toolbox has been initialized.  This typically happens
	// with the Metrowerks SIOUX console when we're running the test
	// suites.
	if (s_ToolboxIsInitialized)
	{
		::CopyCStringToPascal(m_LogBuffer, (StringPtr) m_LogBuffer);
	
		::ParamText((StringPtr) m_LogBuffer, NULL, NULL, NULL);
		if (isError)
    		(void) ::StopAlert(2001, nil);
    	else
    		(void) ::CautionAlert(2001, nil);
    
    	::CopyPascalStringToC((StringPtr) m_LogBuffer, m_LogBuffer);
	}
	else
	{
		std::cerr << std::endl;
		if (isError)
			std::cerr << "ERROR: ";
		else
			std::cerr << "INFO: ";
		std::cerr << m_LogBuffer << std::endl;
	}
}

#elif FIVEL_PLATFORM_OTHER

void TLogger::AlertBuffer(bool isError /* = false */)
{
	PrepareToDisplayError();

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

void TLogger::PrepareToDisplayError()
{
	if (s_ErrorPrepFunction)
		(*s_ErrorPrepFunction)();	
}

void TLogger::RegisterErrorPrepFunction(void (*inFunc)())
{
	s_ErrorPrepFunction = inFunc;	
}

// This routine is declared in TCommon.h.
void FiveLCheckAssertion(int inTest, const char *inDescription,
						 const char *inFile, int inLine)
{
	if (!inTest)
	{
		// Log a fatal error and bail.
		gLog.FatalError("ASSERTION FAILURE: %s:%d: %s",
						inFile, inLine, inDescription);
		abort();
	}
}


/*
 $Log$
 Revision 1.5.4.1  2002/06/15 01:06:32  emk
 3.3.4.7 - Carbonization of Mac build, support for running non-Carbonized build
 in MacOS X's OS 9 emulator, and basic support for 5L.prefs on the Mac.  The
 Carbon build isn't yet ready for prime time--see BugHunt for details--but it
 is good enough to use for engine development.

 * Language changes

   - CHECKDISC is gone; use CHECKVOL instead.
   - EJECT is disabled in the Carbon build, because Carbon has no way to
     identify CD drives reliably.  EJECT still works in the regular build.
   - Gamma fades are ignored in the Carbon build.
   - KEYBINDs must now be accessed with the Command key only--not Option.

 * Things to test

 Please be hugely brutal to 5L; this is a big update.

   - 8-bit systems, palettes, ORIGIN, EJECT on the non-Carbon build.

 * Internal changes

   - TException class (and all subclasses) now take a __FILE__ and __LINE__
     parameter.  This is ugly, but it allows me to debug 5L exceptions even
     without a working debugger (under the OS 9 emulator, for example).
   - FileSystem::Path::(DoesExist|IsRegularFile|IsDirectory) now rely on
     native MacOS File Manager calls instead of the broken MSL stat()
     function (which fails in the OS 9 emulator).
   - The ImlUnit test harness flushes its output more often.
   - Many data structure accessors (and such functions as c2pstr) have been
     replaced by their Carbon equivalents.
   - We now use PowerPlant accessors to get at the QuickDraw globals.
   - We now use PowerPlant calls in place of ValidRect and InvalRect.
   - Some very nasty code which set the palettes of our offscreen GWorlds
     has been removed (offscreen GWorlds have CLUTs, not palettes!).
     The various drawing commands now use gPaletteManager to map indexes
     to RGBColor values, and RGBForeColor to set the color--no more calls
     to ::PmForeColor on offscreen GWorlds, thank you!
   - The CMenuUtil code (which used low-memory system globals to hide
     and show the menu bar) has been removed entirely and replaced by
     calls to HideMenuBar and ShowMenuBar (which are present in 8.5 and
     Carbon).  This is much simpler, nicer, more portable and safer.
   - A bunch of code which had been disabled with #ifdefs has been
     removed entirely.  This mostly related to palettes and an obsolete
     version of the fade code which used GWorlds.
   - Code which used ROM-based KCHR resources to map option keys back to
     their unmodified key caps has been removed.  This means KEYBINDs
     can only be accessed using the Command key.
   - We assume Carbon systems always support the HFS file system (duh).
   - We use PowerPlant glue to access either StandardFile or Navigation
     Services, under OS 8/9 and Carbon, respectively.
   - Some old subroutines in CModuleManager appeared to have been
     snarfed from More Files, an old Mac utility library.  These have
     been moved into MoreFiles.{h,cpp}.

 * Known Carbon Problems

 Fades, ejecting CD-ROMs and playing QuickTime movies are all broken in
 the Carbon build.  Douglas has found a problem with ORIGIN.  It looks
 like we should continue to ship the OS 9 build for use with MacOS X,
 at least for next few months.

 Revision 1.5  2002/05/29 09:38:53  emk
 Fixes for various "crash on exit" bugs in 5L.

   * Fixed lots of bugs in TBTree, mostly in the code for removing nodes.
     TBTree should now work more or less correctly.
   * Removed the broken reference counting logic in TIndex and TIndexFile.
   * Made FatalError call abort(), not exit(1), so the destructors for
     (possibly corrupt) global variables will not be called.

 This code may break either the Windows or Mac build; I'll try to fix things
 right away.

 Revision 1.4  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.3.4.4  2002/05/15 08:13:15  emk
 3.3.2.8 - Overhauled assertion handling to call FatalError and log problems in 5L.log.  Also added hooks for unfading the screen before displaying errors (this is needed to play nicely with the Mac gamma fader).

 Made tweaks to support the migration of Mac (buttpcx ...) to the new anti-aliased typography library.

 The TBTree destructor is still a broken nightmare, especially on FatalError's forced shutdowns.  Expect *both* FiveL's to do something childish immediately after fatal errors and assertion failures.

 Revision 1.3.4.3  2002/04/23 13:25:40  emk
 Merged some KLogger (Mac) features into TLogger (portable).

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
