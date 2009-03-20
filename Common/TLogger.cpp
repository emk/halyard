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

#include "TVersion.h"
#include "TInterpreter.h"
#include "FileSystem.h"
#include "TLog.h"

using namespace Halyard;

TLogger Halyard::gLog;

// This log is used to log ordinary, relatively important events.  This
// file typically exists on a normal user's system.
static TLog gHalyardLog;

// This log is used to log low-level debugging events.  This file typically
// exists on a developer's system.
static TLog gDebugLog;


//=========================================================================
//  Assertion support
//=========================================================================

// This routine is declared in TCommon.h.
void HalyardCheckAssertion(int inTest, const char *inDescription,
                           const char *inFile, int inLine)
{
	if (!inTest)
		// Log a fatal error and bail.
		gLog.Fatal("halyard", "ASSERTION FAILURE: %s:%d: %s",
                   inFile, inLine, inDescription);
}


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
    // We need to let GCC know that this function never actually returns.
    abort();
}


//=========================================================================
//  TLogger static interface
//=========================================================================
//  This is inherited from our old TLogger class, and we may want to tweak
//  it a bit.

TLogger::AlertDisplayFunction TLogger::s_AlertDisplayFunction = NULL;
TLogger::ExitPrepFunction TLogger::s_ExitPrepFunction = NULL;
bool TLogger::s_IsStandardErrorAvailable = true;
std::ostream *TLogger::s_ErrorOutput = NULL;

void TLogger::OpenStandardLogs(bool inShouldOpenDebugLog /*= false*/)
{
	// Initialize the global log file.
	gHalyardLog.Init(SHORT_NAME, true, true);

    // Initialize the debug log if we've been asked to.
	if (inShouldOpenDebugLog)
		gDebugLog.Init("Debug");

    // Print our version string to our logs.
	gLog.Info("%s", VERSION_STRING);
}

void TLogger::OpenRemainingLogsForCrash()
{
    if (!gDebugLog.IsOpen())
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

/// Display an alert on the console.  THIS ROUTINE MAY NOT USE 'ASSERT' OR
/// 'FatalError', BECAUSE IS CALLED BY THE ERROR-LOGGING CODE!
static void ConsoleAlert(TLogger::AlertType inType, const char *inMessage)
{
	std::ostream *out(TLogger::GetErrorOutput());
	*out << std::endl;
    switch (inType) {
		case TLogger::ALERT_INFO:    *out << "INFO: ";    break;
        case TLogger::ALERT_WARNING: *out << "WARNING: "; break;
        case TLogger::ALERT_ERROR:   *out << "ERROR: ";   break;
    }
	*out << inMessage << std::endl << std::flush;
}

void TLogger::DisplayAlert(AlertType inType, const char *inMessage)
{
	if (s_AlertDisplayFunction && !TInterpreterManager::IsInCommandLineMode())
		(*s_AlertDisplayFunction)(inType, inMessage);
    else
        ConsoleAlert(inType, inMessage);
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
