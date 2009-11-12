// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
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

#include <iostream>
#include <fstream>
#include <log4cplus/logger.h>
#include <log4cplus/configurator.h>

#include "TVersion.h"
#include "TInterpreter.h"
#include "FileSystem.h"
#include "CrashReporter.h"
#include "TSystem.h"

#ifdef HAVE__VSNPRINTF
#   define vsnprintf _vsnprintf
#endif

using namespace Halyard;
namespace log = log4cplus;

TLogger Halyard::gLog;


//=========================================================================
//  Assertion support
//=========================================================================

// This routine is declared in TCommon.h.
void HalyardAssertionFailed(const char *inDescription,
                            const char *inFile, int inLine)
{
    // Log a fatal error and bail.
    gLog.Fatal("halyard", "ASSERTION FAILURE: %s:%d: %s",
               inFile, inLine, inDescription);
}


//=========================================================================
//  Converting log levels to and from strings
//=========================================================================

TLogger::Level TLogger::LevelFromString(const std::string &inLevelStr) {
    if (inLevelStr == "trace")
        return TLogger::kTrace;
    else if (inLevelStr == "debug")
        return TLogger::kDebug;
    else if (inLevelStr == "info")
        return TLogger::kInfo;
    else if (inLevelStr == "warn")
        return TLogger::kWarn;
    else if (inLevelStr == "error")
        return TLogger::kError;
    else if (inLevelStr == "fatal")
        return TLogger::kFatal;
    else
        gLog.Fatal("halyard", "Unknown logging level: %s", inLevelStr.c_str());
}

std::string TLogger::StringFromLevel(Level inLevel) {
    switch (inLevel) {
        case kTrace: return "trace";
        case kDebug: return "debug";
        case kInfo:  return "info";
        case kWarn:  return "warn";
        case kError: return "error";
        case kFatal: return "fatal";
        default:
            gLog.Fatal("halyard", "Unknown log level");
    }
}

/// Internal: Translate to log4cplus log level.
static log::LogLevel Log4CPlusLevelFromLevel(TLogger::Level inLevel) {
    switch (inLevel) {
        case TLogger::kTrace: return log::TRACE_LOG_LEVEL;
        case TLogger::kDebug: return log::DEBUG_LOG_LEVEL;
        case TLogger::kInfo:  return log::INFO_LOG_LEVEL;
        case TLogger::kWarn:  return log::WARN_LOG_LEVEL;
        case TLogger::kError: return log::ERROR_LOG_LEVEL;
        case TLogger::kFatal: return log::FATAL_LOG_LEVEL;
        default:
            gLog.Fatal("halyard", "Unknown log level");
    }    
}


//=========================================================================
//  TLogger initialization
//=========================================================================

bool TLogger::sLogFilesAreInitialized = false;
bool TLogger::sShouldTryToAvoidExitingWithError = false;

/// If inPath exists, add it to the crash reporter's list of log files.
static void RegisterLogFileWithCrashReporter(const FileSystem::Path &inPath) {
    if (inPath.DoesExist()) {
        CrashReporter *cr(CrashReporter::GetInstance());
        cr->AddDiagnosticFile(inPath.ToNativePathString(), "log file");
    }
}

void TLogger::InitializeLogFiles() {
    // log4cplus can substitute environment variables into a *.properties
    // file.  We use this mechanism to specify our LOG_DIR.
    FileSystem::Path log_dir(FileSystem::GetAppDataDirectory());
    SetEnvVar("LOG_DIR", log_dir.ToNativePathString());

    // Set up our logging library.  We pass fShadowEnvironment, which
    // allows the user to perform simple variable substitutions from within
    // the *.properties file by shadowing the environment variable
    // namespace with property definitions.
    //
    // Note that GetBaseDirectory() only does something useful if the code
    // in HalyardApp::OnInit was able to guess where our program was
    // located before it was officially opened.  But this will be the case
    // when we're launched in runtime mode, because our working directory
    // will be the root directory of our script.
    FileSystem::Path config(FileSystem::GetBaseDirectory() /
                            "config" / "log4cplus.properties");
    if (!config.DoesExist()) {
        // We don't have a config/log4cplus.properties file in our script,
        // so look for the one associated with our engine.
        config = (FileSystem::GetRuntimeDirectory() /
                  "config" / "log4cplus.properties");
    }
    unsigned flags(log::PropertyConfigurator::fShadowEnvironment);
    log::PropertyConfigurator::doConfigure(config.ToNativePathString(),
                                           log::Logger::getDefaultHierarchy(),
                                           flags);

    // Now we're ready to start logging.
    sLogFilesAreInitialized = true;

    // Print our version string to our logs.
    gLog.Info("halyard", "Launched %s", VERSION_STRING);

    // Add our log files to our crash report.
    RegisterLogFileWithCrashReporter(log_dir / "Halyard.log");
    RegisterLogFileWithCrashReporter(log_dir / "Debug.log");
    RegisterLogFileWithCrashReporter(log_dir / "Trace.log");
}

void TLogger::TryToAvoidExitingWithError() {
    ASSERT(TInterpreterManager::IsInCommandLineMode());
    sShouldTryToAvoidExitingWithError = true;
}


//=========================================================================
//  TLogger::vLog (and support code)
//=========================================================================
//  This does all the heavy lifting.

// Allow big log messages, just in case there are stack traces involved.
enum { LOG_BUFFER_SIZE = 10240 };

void TLogger::vLog(Level inLevel, const std::string &inCategory,
                   const char *inFormat, va_list inArgs)
{
    // Write our log message to a string.  Some constraints: (1) we want
    // our buffering code to be re-entrant. (2) We don't want to allocate a
    // big buffer on the stack. (3) We don't want to depend on vsnprintf
    // having a sane return value, because the MSDN documentation
    // (incorrectly?) says it doesn't.  (4) We don't want to allocate a
    // huge std::string, because then we'd just have to clear it.
    scoped_array<char> buffer(new char[LOG_BUFFER_SIZE]);
    vsnprintf(buffer.get(), LOG_BUFFER_SIZE, inFormat, inArgs);
    buffer[LOG_BUFFER_SIZE-1] = '\0';
    std::string message(buffer.get());
    buffer.reset();

    // If somebody else wants to handle this log message for us, let them.
    if (MaybeDelegateLogMessage(inLevel, inCategory, message))
        return;

    // If appropriate, display an alert containing the log message.  Note
    // that we do this before actually trying to log anything, because
    // DisplayAlert is prohibited from calling back into the logging
    // machinery, whereas some of latter parts of logging are not so
    // well-behaved.
    MaybeDisplayAlert(inLevel, inCategory, message);

    // Write the message to our logging library.  (We tried to do
    // initialization on demand, but there were a few early trace messages
    // that were logged before we could successfully initialize our logs.)
    if (sLogFilesAreInitialized) {
        log::Logger logger(log::Logger::getInstance(inCategory));
        logger.log(Log4CPlusLevelFromLevel(inLevel), message);
    }

    // Decide whether or not we should exit the program.
    MaybeExitWithError(inLevel, inCategory, message);
}

bool TLogger::MaybeDelegateLogMessage(Level inLevel,
                                      const std::string &inCategory,
                                      const std::string &inMessage)
{
    // Log messages below the level of kWarning are never directly
    // visible to the user, so there's no need for our interpreter to
    // be able to handle them in a special way. Similarly, fatal errors
    // are always fatal, and there's no point in letting our
    // interpreter do anything unusual with them.
    if (inLevel < kWarn || inLevel == kFatal)
        return false;

    // If the interpreter makes logging calls from inside
    // MaybeHandleLogMessage, we don't want to pass them back to
    // MaybeHandleLogMessage again.
    if (mIsInMaybeHandleLogMessage)
        return false;

    // If we don't have an interpreter, handle our log message normally.
    if (!TInterpreter::HaveInstance())
        return false;

    // Give TInterpreter a crack at this first (this allows the unit tests,
    // for example, to override the behavior of WARNING).  If TInterpreter
    // doesn't want to handle it, treat it normally.
    bool result = false;
    mIsInMaybeHandleLogMessage = true;
    try {
        TInterpreter *interp(TInterpreter::GetInstance());
        result = interp->MaybeHandleLogMessage(StringFromLevel(inLevel),
                                               inCategory, inMessage);
    } catch (...) {
        mIsInMaybeHandleLogMessage = false;
        throw;
    }
    mIsInMaybeHandleLogMessage = false;
    return result;
}

void TLogger::MaybeDisplayAlert(Level inLevel, const std::string &inCategory,
                                const std::string &inMessage)
{
    if (inLevel >= kError)
        DisplayAlert(ALERT_ERROR, inMessage.c_str());
    else if (inLevel >= kWarn && !TInterpreterManager::IsInRuntimeMode())
        DisplayAlert(ALERT_WARNING, inMessage.c_str());
}

void TLogger::MaybeExitWithError(Level inLevel, const std::string &inCategory,
                                 const std::string &inMessage)
{
    // I'm not sure if this SCRIPT_CRASH / APPLICATION_CRASH distinction
    // actually makes much sense, but it's what the old TLogger code did.
    if (inLevel == kFatal)
        ExitWithError(APPLICATION_CRASH, inMessage);
    else if (inLevel >= kError && !TInterpreterManager::IsInAuthoringMode() &&
             !sShouldTryToAvoidExitingWithError)
        ExitWithError(SCRIPT_CRASH, inMessage);
}

void TLogger::ExitWithError(CrashType inType, const std::string &inMessage) {
    TLogger::PrepareToExit();
    if (!CrashReporter::HaveInstance() ||
        TInterpreterManager::IsInCommandLineMode()) {
        exit(1);
    } else {
        CrashReporter::GetInstance()->CrashNow(inMessage.c_str(), inType);
        // We shouldn't get here, but just in case.
        abort();
    }
}

bool TLogger::IsEnabledFor(Level inLevel, const std::string &inCategory) {
    log::Logger logger(log::Logger::getInstance(inCategory));
    return logger.isEnabledFor(inLevel);
}


//=========================================================================
//  TLogger wrapper methods
//=========================================================================
//  These provide higher-level interfaces than TLogger::vLog.

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
static void ConsoleAlert(TLogger::AlertType inType, const char *inMessage) {
    std::ostream *out(TLogger::GetErrorOutput());
    *out << std::endl;
    switch (inType) {
        case TLogger::ALERT_INFO:    *out << "INFO: ";    break;
        case TLogger::ALERT_WARNING: *out << "WARN: ";    break;
        case TLogger::ALERT_ERROR:   *out << "ERROR: ";   break;
    }
    *out << inMessage << std::endl << std::flush;
}

void TLogger::DisplayAlert(AlertType inType, const char *inMessage) {
    if (s_AlertDisplayFunction && !TInterpreterManager::IsInCommandLineMode())
        (*s_AlertDisplayFunction)(inType, inMessage);
    else
        ConsoleAlert(inType, inMessage);
}

void TLogger::RegisterAlertDisplayFunction(AlertDisplayFunction inFunc) {
    s_AlertDisplayFunction = inFunc;
}

void TLogger::PrepareToExit() {
    if (s_ExitPrepFunction)
        (*s_ExitPrepFunction)();    
}

void TLogger::RegisterExitPrepFunction(ExitPrepFunction inFunc) {
    s_ExitPrepFunction = inFunc;
}
