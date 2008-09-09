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

TLogger::AlertDisplayFunction TLogger::s_AlertDisplayFunction = NULL;
TLogger::ExitPrepFunction TLogger::s_ExitPrepFunction = NULL;
bool TLogger::s_IsStandardErrorAvailable = true;
std::ostream *TLogger::s_ErrorOutput = NULL;

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
	ASSERT(!m_LogOpen);

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

		if (!m_Log.fail())
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
	if (!ShouldLog(Mask))
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
	AlertBuffer(LEVEL_ERROR);
	LogBuffer(ERROR_HEADER);
    if (!TInterpreterManager::IsInAuthoringMode())
        ExitWithError(SCRIPT_CRASH);
}

void TLogger::Caution(const char *Format, ...)
{
	FORMAT_MSG(Format);
	if (!TInterpreterManager::IsInRuntimeMode())
		AlertBuffer(LEVEL_CAUTION);
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
	AlertBuffer(LEVEL_ERROR);
    LogBuffer(FATAL_HEADER);
    ExitWithError(APPLICATION_CRASH);
}

void TLogger::EnvironmentError(const char *Format, ...)
{
    // Format and display our message, and exit without submitting
    // a crash report.
    FORMAT_MSG(Format);
    AlertBuffer(LEVEL_ERROR);
    exit(1);
}

void TLogger::ExitWithError(CrashType inType) {
    if (TInterpreterManager::IsInCommandLineMode()) {
        PrepareToExit();
        exit(1);
    } else {
        CrashNow(inType);
    }
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

/// Display an alert on the console.  THIS ROUTINE MAY NOT USE 'ASSERT' OR
/// 'FatalError', BECAUSE IS CALLED BY THE ERROR-LOGGING CODE!
static void ConsoleAlert(TLogger::LogLevel inLevel, const char *inMessage)
{
	std::ostream *out(TLogger::GetErrorOutput());
	*out << std::endl;
    switch (inLevel) {
		case TLogger::LEVEL_LOG:     *out << "LOG: ";     break;
        case TLogger::LEVEL_CAUTION: *out << "CAUTION: "; break;
        case TLogger::LEVEL_ERROR:   *out << "ERROR: ";   break;
    }
	*out << inMessage << std::endl << std::flush;
}

void TLogger::AlertBuffer(LogLevel inLogLevel /* = LEVEL_LOG */)
{
    if (TInterpreterManager::IsInCommandLineMode())
        // If we're in command-line mode, always display on the console.
        ConsoleAlert(inLogLevel, m_LogBuffer);
    else
        // Check to see if we have a GUI alert dialog, and use it if we
        // can.  If not, fall back to ConsoleAlert.
        DisplayAlert(inLogLevel, m_LogBuffer);
}

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

void TLogger::SetIsStandardErrorAvailable(bool inIsAvailable) {
    s_IsStandardErrorAvailable = inIsAvailable;
}

std::ostream *TLogger::GetErrorOutput() {
    ASSERT(!TInterpreterManager::IsInRuntimeMode());
    if (s_IsStandardErrorAvailable) {
        // We have a perfectly good std::cerr, so let's use it.
        return &std::cerr;
    } else if (!TInterpreterManager::HaveInstance() ||
               !TInterpreterManager::GetInstance()->ScriptHasBegun()) {
        // We don't have a good std::cerr, but we can't call
        // GetScriptTempDirectory yet, so there's nothing useful we can
        // actually do.
        return &std::cerr;
    } else {
        // We should be able to write our output to a file now.
        if (!s_ErrorOutput) {
            // Allocate a std::ofstream for our output.
            FileSystem::Path dir(FileSystem::GetScriptTempDirectory());
            FileSystem::Path file(dir.AddComponent("output.txt"));
            s_ErrorOutput =
                new std::ofstream(file.ToNativePathString().c_str(),
                                  std::ios_base::out | std::ios_base::trunc);
        }
        return s_ErrorOutput;
    }
}

void TLogger::DisplayAlert(LogLevel inLevel, const char *inMessage)
{
	if (s_AlertDisplayFunction)
		(*s_AlertDisplayFunction)(inLevel, inMessage);
    else
        ConsoleAlert(inLevel, inMessage);
}

void TLogger::RegisterAlertDisplayFunction(AlertDisplayFunction inFunc)
{
	s_AlertDisplayFunction = inFunc;
}

void TLogger::PrepareToExit()
{
	if (s_ExitPrepFunction)
		(*s_ExitPrepFunction)();	
}

void TLogger::RegisterExitPrepFunction(ExitPrepFunction inFunc)
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
