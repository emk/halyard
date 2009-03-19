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

#if !defined (_TLogger_h_)
#define _TLogger_h_

#include <cstdarg>
#include <fstream>

#include "FileSystem.h"

BEGIN_NAMESPACE_HALYARD

/// This class provides a unified public interface to our logging
/// subsystem.  This way, callers only need to know their log level, not
/// where messages wind up.
class TLogger : boost::noncopyable {
public:
    enum Level {
        kTrace, //< Primitive calls and other low-level trace messages
        kDebug, //< Script-level debugging information
        kInfo,  //< Information about the system (use sparingly)
        kWarn,  //< Warnings for developers
        kError, //< Serious errors (fatal when not in developer mode)
        kFatal  //< Fatal errors
    };

    TLogger() {}
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
    void Fatal(const std::string &inCategory, const char *inFormat, ...)
        __attribute__((noreturn));
};

/// Our centralized logging interface.
extern TLogger gLog;

#define LOG_BUFFER_SIZE	10240

//////////
/// A class for logging output to a file.
///
/// \author Chuck Officer
/// \author Sean Sharp
/// \author ...and others
///
class TLog
{
public:
    /// The severity of an alert dialog.
    enum LogLevel {
        LEVEL_LOG,
        LEVEL_WARNING,
        LEVEL_ERROR
    };

    /// A function which (very carefully) displays an alert dialog and
    /// waits until the user clicks OK.
    typedef void (*AlertDisplayFunction)(LogLevel, const char *);

    /// A function that should be called shortly before crashing the
    /// engine, in a last-ditch effort to restore the user's screen to
    /// reasonable settings.
    typedef void (*ExitPrepFunction)();

	//////////
	/// Constructor.
	///
	TLog();
	
	//////////
	/// Constructor.
	///
	~TLog();

	//////////
	/// Initialize the log file.  Assumes path is current directory.
	///
	/// \param Name  name of the log file (no file extension)
	/// [in_optional] OpenFile - if true opens the file on init (default true)
	/// [in_optional] Append - if true, opens file for append only
    ///                        (defualt false)
	///
	void	Init(const char *Name, bool OpenFile = true, bool Append = false);

	//////////
	/// Initialize the log file.
	///
	/// \param inLogFile  location where the log file should be written
	/// [in_optional] OpenFile - if true, opens the file on init (default true)
	/// [in_optional] Append - if true, opens file for append only
    ///                        (defualt false)
	///
	void	Init(const FileSystem::Path &inLogFile, bool OpenFile = true,
				 bool Append = false);

	//////////
	/// Log a general message.
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	Log(const char *Format, va_list inArgs);	
	
	//////////
	/// Log an error message.  Prepends ERROR_HEADER.
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	Error(const char *Format, va_list inArgs);
	
	//////////
	/// Log a warning message.  Prepends WARNING_HEADER.
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	Warning(const char *Format, va_list inArgs);
	
	//////////
	/// Log a fatal error message.  Prepends FATAL_HEADER and calls Shutdown().
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	FatalError(const char *Format, va_list inArgs)
        __attribute__((noreturn));

	//////////
	/// Put a time stamp in the log.
	///
	void	TimeStamp(void);

private:
	//////////
	/// File output stream
	///
	std::ofstream	m_Log;
	
	//////////
	/// Filename for log output
	///
	std::string m_FileName;
	
	//////////
	/// Log buffer
	///
	char		m_LogBuffer[LOG_BUFFER_SIZE];

    //////////
    /// The most recent lines written to our log file.
    ///
    std::deque<std::string> m_RecentEntries;
	
	//////////
	/// Has the log file been opened for writing?
	///
	bool		m_LogOpen;
	
	//////////
	/// Was there an error when opening the log file for writing?
	///
	bool		m_OpenFailed;
	
	//////////
	/// Is the log output in append mode?
	///
	bool		m_Append;
	
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

    //////////
    /// Add a string to our m_RecentEntries list.
    ///
    void        AddToRecentEntries(const std::string &str);

	//////////
	/// Write the contents of m_LogBuffer to the log file.
	///
	/// \param Header  a header to precede the log buffer contents
	///
	void		LogBuffer(const char *Header);
	
	//////////
	/// Display an alert message box with the contents of m_LogBuffer.
	///
	/// \param inError  Is it an error message?
	///
	void		AlertBuffer(LogLevel inLogLevel = LEVEL_LOG);

    //////////
    /// Exit the engine abruptly with a fatal error.  In COMMAND_LINE mode,
    /// this will exit the engine and return a non-zero result to the
    /// shell.  In other modes, it will try to invoke the CrashReporter.
    ///
    void        ExitWithError(CrashType inType) __attribute__((noreturn));

    //////////
    /// Crash the engine with a fatal error.
    ///
    void        CrashNow(CrashType inType) __attribute__((noreturn));

public:
	//////////
	/// Open up all the log files which will be required by our program.
	///
	/// \param inOpenDebugLog  Should we open up the debugging log as well?
	///
	static void OpenStandardLogs(bool inOpenDebugLog = false);

    //////////
    /// Open up any logs which weren't opened by OpenStandardLogs, and
    /// write the contents of m_RecentEntries to disk.
    ///
    static void OpenRemainingLogsForCrash();

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
    static void DisplayAlert(LogLevel inLevel, const char *inMessage);

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
};

END_NAMESPACE_HALYARD

#endif // _TLogger_h_
