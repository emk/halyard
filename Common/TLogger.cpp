// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

TLogger Halyard::gLog;

// This log is used to log ordinary, relatively important events.  This
// file typically exists on a normal user's system.
static TLog gHalyardLog;

// This log is used to log low-level debugging events.  This file typically
// exists on a developer's system.
static TLog gDebugLog;


//=========================================================================
//  TLogger
//=========================================================================
//  This is our new, unified logging interface.

void TLogger::vLog(Level inLevel, const std::string &inCategory,
                   const char *inFormat, va_list inArgs)
{
    switch (inLevel) {
        case kTrace:
        case kDebug:
            gDebugLog.Log(inFormat, inArgs);
            break;

        case kInfo:
            gDebugLog.Log(inFormat, inArgs);
            gHalyardLog.Log(inFormat, inArgs);
            break;

        case kWarn:
            gDebugLog.Log(inFormat, inArgs);
            gHalyardLog.Warning(inFormat, inArgs);
            break;

        case kError:
            gDebugLog.Log(inFormat, inArgs);
            gHalyardLog.Error(inFormat, inArgs);
            break;

        case kFatal:
        default:
            gDebugLog.Log(inFormat, inArgs);
            gHalyardLog.FatalError(inFormat, inArgs);
            break;
    }
}

#define VLOG_WITH_LEVEL(LEVEL) \
    va_list args; \
    va_start(args, inFormat); \
    vLog((LEVEL), inCategory, inFormat, args);  \
    va_end(args);

void TLogger::Log(Level inLevel, const std::string &inCategory,
                  const char *inFormat, ...)
{
    VLOG_WITH_LEVEL(inLevel);
}

void TLogger::Trace(const std::string &inCategory, const char *inFormat, ...) {
    VLOG_WITH_LEVEL(kTrace);
}

void TLogger::Debug(const std::string &inCategory, const char *inFormat, ...) {
    VLOG_WITH_LEVEL(kDebug);
}

void TLogger::Info(const std::string &inCategory, const char *inFormat, ...) {
    VLOG_WITH_LEVEL(kInfo);
}

void TLogger::Warn(const std::string &inCategory, const char *inFormat, ...) {
    VLOG_WITH_LEVEL(kWarn);
}

void TLogger::Error(const std::string &inCategory, const char *inFormat, ...) {
    VLOG_WITH_LEVEL(kError);
}

void TLogger::Fatal(const std::string &inCategory, const char *inFormat, ...) {
    VLOG_WITH_LEVEL(kFatal);
}



//=========================================================================
//  TLog
//=========================================================================
//  This is our older logging class, modified to be called from our newer,
//  unified TLogger interface.

TLog::AlertDisplayFunction TLog::s_AlertDisplayFunction = NULL;
TLog::ExitPrepFunction TLog::s_ExitPrepFunction = NULL;
bool TLog::s_IsStandardErrorAvailable = true;
std::ostream *TLog::s_ErrorOutput = NULL;

#define FATAL_HEADER	"Fatal Error: "
#define ERROR_HEADER	"Error: "
#define WARNING_HEADER	"Warning: "

#define MAX_RECENT_ENTRIES (100)

#ifdef HAVE__VSNPRINTF
#	define vsnprintf _vsnprintf
#endif

#define FORMAT_MSG(FORMAT,ARGS) \
	vsnprintf(m_LogBuffer, LOG_BUFFER_SIZE, (FORMAT), (ARGS)); \
	m_LogBuffer[LOG_BUFFER_SIZE-1] = 0; \

TLog::TLog()
{ 
	m_LogOpen = false; 
	m_OpenFailed = false;
	m_Append = false;
	m_LogMask = LOG_ALL;
}

TLog::~TLog()
{
	if (m_LogOpen)
		m_Log.close();
}

//
//	Init - Initialize the log file. 
//
void TLog::Init(const FileSystem::Path &inLogFile, 
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
void TLog::Init(const char *Name, bool OpenFile /* = true */,
				   bool Append /* = false */)
{	
	Init(FileSystem::GetAppDataDirectory().AddComponent(Name),
         OpenFile, Append);
}

void TLog::Log(int32 Mask, const char *Format, va_list inArgs)
{
	if (!ShouldLog(Mask))
		return;

	FORMAT_MSG(Format, inArgs);
	LogBuffer(NULL);	
}

void TLog::Log(const char *Format, va_list inArgs)
{
	FORMAT_MSG(Format, inArgs);
	LogBuffer(NULL);
}

void TLog::Error(const char *Format, va_list inArgs)
{
	FORMAT_MSG(Format, inArgs);
	AlertBuffer(LEVEL_ERROR);
	LogBuffer(ERROR_HEADER);
    if (!TInterpreterManager::IsInAuthoringMode())
        ExitWithError(SCRIPT_CRASH);
}

void TLog::Warning(const char *Format, va_list inArgs)
{
	FORMAT_MSG(Format, inArgs);
    // Give TInterpreter a crack at this first (this allows the unit tests,
    // for example, to override the behavior of WARNING).  If TInterpreter
    // doesn't want to handle it, treat it normally.
    if (!TInterpreter::HaveInstance() ||
        !TInterpreter::GetInstance()->MaybeHandleWarning(m_LogBuffer))
    {
        if (!TInterpreterManager::IsInRuntimeMode())
            AlertBuffer(LEVEL_WARNING);
        LogBuffer(WARNING_HEADER);
    }
}

void TLog::FatalError(const char *Format, va_list inArgs)
{
	// We call AlertBuffer before LogBuffer, because
	// the AlertBuffer code is required NOT to
	// call back into FatalError, whereas LogBuffer
	// relies on a lot of subsystems which might
	// somehow fail.
	FORMAT_MSG(Format, inArgs);
	AlertBuffer(LEVEL_ERROR);
    LogBuffer(FATAL_HEADER);
    ExitWithError(APPLICATION_CRASH);
}

void TLog::ExitWithError(CrashType inType) {
    if (TInterpreterManager::IsInCommandLineMode()) {
        PrepareToExit();
        exit(1);
    } else {
        CrashNow(inType);
    }
}

void TLog::CrashNow(CrashType inType) {
    PrepareToExit();
    CrashReporter::GetInstance()->CrashNow(m_LogBuffer, inType);
    // We shouldn't get here, but just in case.
	abort();
}

void TLog::AddToRecentEntries(const std::string &str)
{
    if (m_RecentEntries.size() == MAX_RECENT_ENTRIES)
        m_RecentEntries.pop_front();
    m_RecentEntries.push_back(str);
}


//
//	LogBuffer - 
//
void TLog::LogBuffer(const char *Header)
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
static void ConsoleAlert(TLog::LogLevel inLevel, const char *inMessage)
{
	std::ostream *out(TLog::GetErrorOutput());
	*out << std::endl;
    switch (inLevel) {
		case TLog::LEVEL_LOG:     *out << "LOG: ";     break;
        case TLog::LEVEL_WARNING: *out << "WARNING: "; break;
        case TLog::LEVEL_ERROR:   *out << "ERROR: ";   break;
    }
	*out << inMessage << std::endl << std::flush;
}

void TLog::AlertBuffer(LogLevel inLogLevel /* = LEVEL_LOG */)
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
void TLog::TimeStamp(void)
{
	time_t	timeNow;
	char	*timeStrPtr;

	time(&timeNow);
	timeStrPtr = ctime(&timeNow);
	m_Log << timeStrPtr << std::endl;
}

void TLog::OpenStandardLogs(bool inShouldOpenDebugLog /*= false*/)
{
	// Initialize the global log file.
	gHalyardLog.Init(SHORT_NAME, true, true);

    // Initialize the debug log if we've been asked to.
	if (inShouldOpenDebugLog)
		gDebugLog.Init("Debug");

    // Print our version string to our logs.
	gLog.Info("%s", VERSION_STRING);
}

void TLog::OpenRemainingLogsForCrash()
{
    if (!gDebugLog.m_LogOpen)
        gDebugLog.Init("DebugRecent");
}

void TLog::SetIsStandardErrorAvailable(bool inIsAvailable) {
    s_IsStandardErrorAvailable = inIsAvailable;
}

std::ostream *TLog::GetErrorOutput() {
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

void TLog::DisplayAlert(LogLevel inLevel, const char *inMessage)
{
	if (s_AlertDisplayFunction)
		(*s_AlertDisplayFunction)(inLevel, inMessage);
    else
        ConsoleAlert(inLevel, inMessage);
}

void TLog::RegisterAlertDisplayFunction(AlertDisplayFunction inFunc)
{
	s_AlertDisplayFunction = inFunc;
}

void TLog::PrepareToExit()
{
	if (s_ExitPrepFunction)
		(*s_ExitPrepFunction)();	
}

void TLog::RegisterExitPrepFunction(ExitPrepFunction inFunc)
{
	s_ExitPrepFunction = inFunc;
}

// This routine is declared in TCommon.h.
void HalyardCheckAssertion(int inTest, const char *inDescription,
                           const char *inFile, int inLine)
{
	if (!inTest)
		// Log a fatal error and bail.
		gLog.Fatal("halyard", "ASSERTION FAILURE: %s:%d: %s",
                   inFile, inLine, inDescription);
}
