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
#include <stdio.h>
#include <stdlib.h>

#include "TInterpreter.h"
#include "CrashReporter.h"
#include "TLog.h"

using namespace Halyard;


//=========================================================================
//  TLog
//=========================================================================
//  This is our older logging class, modified to be called from our newer,
//  unified TLogger interface.

#define FATAL_HEADER	"Fatal Error: "
#define ERROR_HEADER	"Error: "
#define WARNING_HEADER	"Warning: "

#define MAX_RECENT_ENTRIES (100)

TLog::TLog()
{ 
	m_LogOpen = false; 
	m_OpenFailed = false;
	m_Append = false;
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

void TLog::Log(const std::string &inMessage) {
	LogMessage(NULL, inMessage);
}

void TLog::Warning(const std::string &inMessage) {
    if (!TInterpreterManager::IsInRuntimeMode())
        AlertMessage(TLogger::ALERT_WARNING, inMessage);
    LogMessage(WARNING_HEADER, inMessage);
}

void TLog::Error(const std::string &inMessage) {
	AlertMessage(TLogger::ALERT_ERROR, inMessage);
	LogMessage(ERROR_HEADER, inMessage);
    if (!TInterpreterManager::IsInAuthoringMode())
        ExitWithError(SCRIPT_CRASH, inMessage);
}

void TLog::FatalError(const std::string &inMessage) {
	// We call AlertMessage before LogMessage, because
	// the AlertMessage code is required NOT to
	// call back into FatalError, whereas LogMessage
	// relies on a lot of subsystems which might
	// somehow fail.
	AlertMessage(TLogger::ALERT_ERROR, inMessage);
    LogMessage(FATAL_HEADER, inMessage);
    ExitWithError(APPLICATION_CRASH, inMessage);
}

void TLog::ExitWithError(CrashType inType, const std::string &inMessage) {
    if (TInterpreterManager::IsInCommandLineMode()) {
        TLogger::PrepareToExit();
        exit(1);
    } else {
        CrashNow(inType, inMessage);
    }
}

void TLog::CrashNow(CrashType inType, const std::string &inMessage) {
    TLogger::PrepareToExit();
    CrashReporter::GetInstance()->CrashNow(inMessage.c_str(), inType);
    // We shouldn't get here, but just in case.
	abort();
}

void TLog::AddToRecentEntries(const std::string &str) {
    if (m_RecentEntries.size() == MAX_RECENT_ENTRIES)
        m_RecentEntries.pop_front();
    m_RecentEntries.push_back(str);
}


//
//	LogMessage - 
//
void TLog::LogMessage(const char *Header, const std::string &inMessage) {
    // Build our complete log message.
    std::string msg;
    if (Header != NULL)
        msg = Header;
    msg += inMessage;

	if (m_LogOpen)
        // If our log file is open, write our message to it.
        m_Log << msg << std::endl << std::flush;
    else
        // Otherwise, record our log message in m_RecentEntries.
        AddToRecentEntries(msg);
}

void TLog::AlertMessage(TLogger::AlertType inType, const std::string &inMessage)
{
    TLogger::DisplayAlert(inType, inMessage.c_str());
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
