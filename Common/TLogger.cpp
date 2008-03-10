// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#include "CommonHeaders.h"

#include <time.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "TLogger.h"
#include "TVersion.h"
#include "TInterpreter.h"
#include "CrashReporter.h"

using namespace Halyard;

void (*TLogger::s_ErrorPrepFunction)() = NULL;
void (*TLogger::s_ExitPrepFunction)() = NULL;


#ifdef APP_PLATFORM_MACINTOSH
bool TLogger::s_ToolboxIsInitialized = false;
#endif

#define FATAL_HEADER	"Fatal Error: "
#define ERROR_HEADER	"Error: "
#define CAUTION_HEADER	"Caution: "

#define MAX_RECENT_ENTRIES (100)

#ifdef HAVE__VSNPRINTF
#	define vsnprintf _vsnprintf
#endif

#define FORMAT_MSG(Format)	\
	va_list	argPtr;			\
	va_start(argPtr, (Format));		\
	vsnprintf(m_LogBuffer, LOG_BUFFER_SIZE, (Format), argPtr);	\
	m_LogBuffer[LOG_BUFFER_SIZE-1] = 0; \
	va_end(argPtr)

// Standard logs.
TLogger Halyard::gLog;
TLogger Halyard::gDebugLog;

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
			m_Log.open(m_FileName.c_str(), std::ios::out | std::ios::app);
		else
			m_Log.open(m_FileName.c_str(), std::ios::out);

		if (not m_Log.fail())
			m_LogOpen = true;
		else
			m_OpenFailed = true;

        if (m_LogOpen) {
            CrashReporter::GetInstance()->AddDiagnosticFile(m_FileName,
                                                            "log file");
            if (m_Append)
            {
                // put welcome line
                m_Log << std::endl;
                TimeStamp();
            }

            // Write out any log entries that we've accumulated in memory.
            std::deque<std::string>::iterator iter = m_RecentEntries.begin();
            for (; iter != m_RecentEntries.end(); ++iter)
                m_Log << *iter << std::endl;
            m_Log << std::flush;
            m_RecentEntries.clear();
        }
	}
}

//
//	Init - Initialize the log file. Use the current directory.
//
void TLogger::Init(const char *Name, bool OpenFile /* = true */,
				   bool Append /* = false */)
{	
	Init(FileSystem::GetAppDataDirectory().AddComponent(Name),
         OpenFile, Append);
}

void TLogger::Log(int32 Mask, const char *Format, ...)
{
	if (not ShouldLog(Mask))
		return;

	FORMAT_MSG(Format);
	LogBuffer(NULL);	
}

void TLogger::Log(const char *Format, ...)
{
	FORMAT_MSG(Format);
	LogBuffer(NULL);
}

void TLogger::Error(const char *Format, ...)
{
	FORMAT_MSG(Format);
	AlertBuffer(true);
	LogBuffer(ERROR_HEADER);
    if (TInterpreterManager::IsInRuntimeMode())
        CrashNow(SCRIPT_CRASH);
}

void TLogger::Caution(const char *Format, ...)
{
	FORMAT_MSG(Format);
	if (m_CautionAlert)
		AlertBuffer(false);
	LogBuffer(CAUTION_HEADER);
}

void TLogger::FatalError(const char *Format, ...)
{
	// We call AlertBuffer before LogBuffer, because
	// the AlertBuffer code is required NOT to
	// call back into FatalError, whereas LogBuffer
	// relies on a lot of subsystems which might
	// somehow fail.
	FORMAT_MSG(Format);
	AlertBuffer(true);
    LogBuffer(FATAL_HEADER);
    CrashNow(APPLICATION_CRASH);
}

void TLogger::EnvironmentError(const char *Format, ...)
{
    // Format and display our message, and exit without submitting
    // a crash report.
    FORMAT_MSG(Format);
    AlertBuffer(true);
    exit(1);
}

void TLogger::CrashNow(CrashType inType) {
    PrepareToExit();
    CrashReporter::GetInstance()->CrashNow(m_LogBuffer, inType);
    // We shouldn't get here, but just in case.
	abort();
}

void TLogger::AddToRecentEntries(const std::string &str)
{
    if (m_RecentEntries.size() == MAX_RECENT_ENTRIES)
        m_RecentEntries.pop_front();
    m_RecentEntries.push_back(str);
}


//
//	LogBuffer - 
//
void TLogger::LogBuffer(const char *Header)
{
#ifdef APP_PLATFORM_MACINTOSH
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
#endif // APP_PLATFORM_*

    // Build our complete log message.
    std::string msg;
    if (Header != NULL)
        msg = Header;
    msg += m_LogBuffer;

	if (m_LogOpen)
        // If our log file is open, write our message to it.
        m_Log << msg << std::endl << std::flush;
    else
        // Otherwise, record our log message in m_RecentEntries.
        AddToRecentEntries(msg);
}

void TLogger::AlertBuffer(bool isError /* = false */)
{
    SafeAlert(isError, m_LogBuffer);
}

//
//	SafeAlert - Display an alert.
//
//  THIS ROUTINE MAY NOT USE 'ASSERT' OR 'FatalError', BECAUSE IS CALLED
//  BY THE ERROR-LOGGING CODE!
//
#ifdef APP_PLATFORM_WIN32

#include <windows.h>

void TLogger::SafeAlert(bool isError, const char *message)
{
	PrepareToDisplayError();

	uint32 alertType = MB_TASKMODAL | MB_OK;
	if (isError)
		alertType |= MB_ICONSTOP;
	else
		alertType |= MB_ICONINFORMATION;

	::MessageBox(NULL, message, NULL, alertType);
}

#elif APP_PLATFORM_MACINTOSH

#include <Dialogs.h>
#include <TextUtils.h>

void TLogger::SafeAlert(bool isError, const char *message)
{
	PrepareToDisplayError();

	// Make sure we don't call StopAlert or CautionAlert before
	// the Toolbox has been initialized.  This typically happens
	// with the Metrowerks SIOUX console when we're running the test
	// suites.
	if (s_ToolboxIsInitialized)
	{
        // PORTABILITY - This code with 'message' will no longer
        // work, because 'message' is now read-only.  This will
        // need to be fixed when we next try the Mac port.
		::CopyCStringToPascal(message, (StringPtr) message);
	
		::ParamText((StringPtr) message, NULL, NULL, NULL);
		if (isError)
    		(void) ::StopAlert(2001, nil);
    	else
    		(void) ::CautionAlert(2001, nil);
    
    	::CopyPascalStringToC((StringPtr) message, message);
	}
	else
	{
		std::cerr << std::endl;
		if (isError)
			std::cerr << "ERROR: ";
		else
			std::cerr << "INFO: ";
		std::cerr << message << std::endl;
	}
}

#elif APP_PLATFORM_OTHER

void TLogger::SafeAlert(bool isError, const char *message);
{
	PrepareToDisplayError();

	std::cerr << std::endl;
	if (isError)
		std::cerr << "ERROR: ";
	else
		std::cerr << "INFO: ";
	std::cerr << message << std::endl;
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

	if (inShouldOpenDebugLog)
	{
		// Initialize the debug log.
		gDebugLog.Init("Debug");
		gDebugLog.Log("%s", VERSION_STRING);
	}	
}

void TLogger::OpenRemainingLogsForCrash()
{
    if (!gDebugLog.m_LogOpen)
        gDebugLog.Init("DebugRecent");
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

void TLogger::PrepareToExit()
{
	if (s_ExitPrepFunction)
		(*s_ExitPrepFunction)();	
}

void TLogger::RegisterExitPrepFunction(void (*inFunc)())
{
	s_ExitPrepFunction = inFunc;	
}

// This routine is declared in TCommon.h.
void HalyardCheckAssertion(int inTest, const char *inDescription,
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
