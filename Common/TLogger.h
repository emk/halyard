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

#ifndef TLogger_H
#define TLogger_H

#include <cstdarg>

BEGIN_NAMESPACE_HALYARD

/// This class provides a unified public interface to our logging
/// subsystem.  This way, callers only need to know their log level, not
/// where messages wind up.
class TLogger : boost::noncopyable {
public:
    // These log levels correspond to the log levels used by log4j.
    enum Level {
        kTrace, //< Primitive calls and other low-level trace messages
        kDebug, //< Script-level debugging information
        kInfo,  //< Information about the system (use sparingly)
        kWarn,  //< Warnings for developers
        kError, //< Serious errors (fatal when not in developer mode)
        kFatal  //< Fatal errors
    };

    // These alert types generally correspond to different icons in the
    // upper-left corner of a dialog box.
    enum AlertType {
        ALERT_INFO,    //< An informational dialog
        ALERT_WARNING, //< A warning dialog
        ALERT_ERROR    //< An error dialog
    };

    /// Convert a string to a log level.
    static Level LevelFromString(const std::string &inLevelStr);

    /// Convert a log level to a string.
    static std::string StringFromLevel(Level inLevel);

private:
    //////////
    /// Have we initialized our log files yet?
    ///
    static bool sLogFilesAreInitialized;

    /// Used to prevent re-entrant calls to MaybeHandleLogMessage.
    bool mIsInMaybeHandleLogMessage;

    /// We call this function to see whether anybody wants to handle the
    /// log message for us.
    bool MaybeDelegateLogMessage(Level inLevel,
                                 const std::string &inCategory,
                                 const std::string &inMessage);

    /// If appropriate, show an alert dialog.
    void MaybeDisplayAlert(Level inLevel, const std::string &inCategory,
                           const std::string &inMessage);

    /// If appropriate, exit the program with an error.
    void MaybeExitWithError(Level inLevel, const std::string &inCategory,
                            const std::string &inMessage);

    // Exit the program with an error.
    void ExitWithError(CrashType inType, const std::string &inMessage)
        __attribute__((noreturn));

public:
    //////////
    /// Initialize TLogger.
    ///
    static void InitializeLogFiles();

    TLogger() : mIsInMaybeHandleLogMessage(false) {}
    virtual ~TLogger() {}

    /// Log a message, and possibly quit the engine or report a crash
    /// if the log level is high enough.
    ///
    /// \param inLevel The severity of this message.
    /// \param inCategory The category of this error (generally the portion
    ///   of the code which caused the problem).  The category "Halyard" and
    ///   any categories beginning with "Halyard." are reserved for the engine.
    /// \param inFormat A printf-style format string.
    void Log(Level inLevel, const std::string &inCategory,
             const char *inFormat, ...);

    /// A version of Log which can be called from another varargs function.
    void vLog(Level inLevel, const std::string &inCategory,
              const char *inFormat, va_list inArgs);

    /// Call Log with a level of kTrace.
    void Trace(const std::string &inCategory, const char *inFormat, ...);
    /// Call Log with a level of kDebug.
    void Debug(const std::string &inCategory, const char *inFormat, ...);
    /// Call Log with a level of kInfo.
    void Info(const std::string &inCategory, const char *inFormat, ...);
    /// Call Log with a level of kWarn.
    void Warn(const std::string &inCategory, const char *inFormat, ...);
    /// Call Log with a level of kError.  May not return.
    void Error(const std::string &inCategory, const char *inFormat, ...);
    /// Call Log with a level of kFatal.  Will never return.
    __declspec(noreturn)
    void Fatal(const std::string &inCategory, const char *inFormat, ...)
        __attribute__((noreturn));

public:
    /// A function which (very carefully) displays an alert dialog and
    /// waits until the user clicks OK.
    typedef void (*AlertDisplayFunction)(AlertType, const char *);

    /// A function that should be called shortly before crashing the
    /// engine, in a last-ditch effort to restore the user's screen to
    /// reasonable settings.
    typedef void (*ExitPrepFunction)();

    //////////
    /// Notfy TLog whether standard error is available.
    ///
    static void SetIsStandardErrorAvailable(bool inIsAvailable);

    //////////
    /// Get an output stream that we can use for displaying errors.  This
    /// will point to either std:cerr, or to a file if std::cerr is not
    /// available.
    ///
    static std::ostream *GetErrorOutput();

    //////////
    /// Display an alert in a plaform-specific fashion.
    ///
    static void DisplayAlert(AlertType inType, const char *inMessage);

    //////////
    /// Register a function to display an alert.  Note that this function
    /// MUST NOT call ASSERT, FatalError, or other logging functions!
    ///
    static void RegisterAlertDisplayFunction(AlertDisplayFunction inFunc);

    //////////
    /// We may need to restore some system state before our application
    /// exits.  Call this function to do anything necessary.
    ///
    static void PrepareToExit();
    
    //////////
    /// Install a function to be called before exiting with an error.
    ///
    static void RegisterExitPrepFunction(ExitPrepFunction inFunc);

private:
	//////////
	/// Either NULL, or a function which can be used to display an alert.
	///
	static AlertDisplayFunction s_AlertDisplayFunction;

	//////////
	/// Either NULL, or a function which should be called before exiting.
	///
	static ExitPrepFunction s_ExitPrepFunction;

    //////////
    /// Can we use std::cerr for printing messages?  This is false for
    /// Win32 and Mac GUI applications, which aren't hooked up to a usable
    /// console.
    ///
    static bool s_IsStandardErrorAvailable;

    //////////
    /// A pointer to the "console" that we'll use for printing errors to
    /// the console.
    ///
    static std::ostream *s_ErrorOutput;    
};

/// Our centralized logging interface.
extern TLogger gLog;

END_NAMESPACE_HALYARD

#endif // TLogger_H
