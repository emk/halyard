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

#ifndef TLog_H
#define TLog_H

#include <fstream>
#include "FileSystem.h"
#include "TLogger.h"

BEGIN_NAMESPACE_HALYARD

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
    /// Is this log open?
    ///
    bool IsOpen() const { return m_LogOpen; }

	//////////
	/// Log a general message.
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	Log(const std::string &inMessage);
	
	//////////
	/// Log a warning message.  Prepends WARNING_HEADER.
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	Warning(const std::string &inMessage);
	
	//////////
	/// Log an error message.  Prepends ERROR_HEADER.
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	Error(const std::string &inMessage);
	
	//////////
	/// Log a fatal error message.  Prepends FATAL_HEADER and calls Shutdown().
	///
	/// \param Format  a printf format string (e.g. "Count is %d.", count)
	///
	void	FatalError(const std::string &inMessage)
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
    /// Add a string to our m_RecentEntries list.
    ///
    void        AddToRecentEntries(const std::string &str);

	//////////
	/// Write the contents of inMessage to the log file.
	///
	/// \param Header  a header to precede the log buffer contents
	///
	void		LogMessage(const char *Header, const std::string &inMessage);
};

END_NAMESPACE_HALYARD

#endif // TLog_H
