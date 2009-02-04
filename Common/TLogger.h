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

#include <fstream>

#include "FileSystem.h"

BEGIN_NAMESPACE_HALYARD

#define LOG_NONE		0x00000000
#define LOG_ALL			0xFFFFFFFF

#define LOG_BUFFER_SIZE	10240

//////////
/// A class for logging output to a file.
///
/// \author Chuck Officer
/// \author Sean Sharp
/// \author ...and others
///
class TLogger
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
	TLogger();
	
	//////////
	/// Constructor.
	///
	~TLogger();

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
	/// \param Mask  a mask to check against the log mask before logging
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	Log(int32 Mask, const char *Format, ...);
	
	//////////
	/// Log a general message.
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	Log(const char *Format, ...);	
	
	//////////
	/// Log an error message.  Prepends ERROR_HEADER.
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	Error(const char *Format, ...);
	
	//////////
	/// Log a warning message.  Prepends WARNING_HEADER.
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	Warning(const char *Format, ...);
	
	//////////
	/// Log a fatal error message.  Prepends FATAL_HEADER and calls Shutdown().
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	FatalError(const char *Format, ...) __attribute__((noreturn));

	//////////
	/// Log an error complaining about the runtime environment.  This is
    /// like "FatalError", except that it doesn't try to report the crash,
    /// since it's (presumably) not our problem.
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	EnvironmentError(const char *Format, ...);
	
	//////////
	/// Put a time stamp in the log.
	///
	void	TimeStamp(void);

	//////////
	/// Turn all logging off.
	///
	inline void LogNothing(void) { m_LogMask = LOG_NONE; }
	
	//////////
	/// Logical "or" the log mask with the given mask.
	///
	/// \param Mask  the mask
	///
	inline void AddLogMask(int32 Mask) { m_LogMask |= Mask; }
	
	//////////
	/// Turn all loggin on.
	///
	inline void LogEverything(void) { m_LogMask = LOG_ALL; }

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
	/// Logger mask
	///
	uint32		m_LogMask;

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

	//////////
	/// Should a log message with a given mask be logged?
	///
	/// \param Mask  a mask to check against
	/// \return  true if the message should be logged, false otherwise
	///
	inline bool	ShouldLog(int32 Mask) 
	{ 
		if (m_LogOpen)
			if ((m_LogMask & Mask) != 0)
				return (true);
		return (false);
	}

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
    /// Notfy TLogger whether standard error is available.
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

//////////
/// This log is used to log ordinary, relatively important events.  This
/// file typically exists on a normal user's system.
///
extern TLogger gLog;

//////////
/// This log is used to log low-level debugging events.  This file typically
/// exists on a developer's system.
///
extern TLogger gDebugLog;

END_NAMESPACE_HALYARD

#endif // _TLogger_h_
