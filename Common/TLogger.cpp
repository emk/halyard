// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

USING_NAMESPACE_FIVEL

void (*TLogger::s_ErrorPrepFunction)() = NULL;
void (*TLogger::s_ExitPrepFunction)() = NULL;


#ifdef FIVEL_PLATFORM_MACINTOSH
bool TLogger::s_ToolboxIsInitialized = false;
#endif

#define FATAL_HEADER	"Fatal Error: "
#define ERROR_HEADER	"Error: "
#define CAUTION_HEADER	"Caution: "

#ifdef HAVE__VSNPRINTF
#	define vsnprintf _vsnprintf
#endif

#define FormatMsg(Format)	\
	va_list	argPtr;			\
	va_start(argPtr, (Format));		\
	vsnprintf(m_LogBuffer, LOG_BUFFER_SIZE, (Format), argPtr);	\
	m_LogBuffer[LOG_BUFFER_SIZE-1] = 0; \
	va_end(argPtr)

// Standard logs.
TLogger FIVEL_NS gLog;
TLogger FIVEL_NS gDebugLog;
TLogger FIVEL_NS gMissingMediaLog;

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
        }
	}
}

//
//	Init - Initialize the log file. Use the current directory.
//
void TLogger::Init(const char *Name, bool OpenFile /* = true */,
				   bool Append /* = false */)
{	
	Init(FileSystem::GetBaseDirectory().AddComponent(Name), OpenFile, Append);
}

void TLogger::Log(int32 Mask, const char *Format, ...)
{
	if (!m_LogOpen)
		return;

	if (not ShouldLog(Mask))
		return;

	FormatMsg(Format);
	LogBuffer(NULL);	
}

void TLogger::Log(const char *Format, ...)
{
	if (!m_LogOpen)
		return;

	FormatMsg(Format);
	LogBuffer(NULL);
}

void TLogger::Error(const char *Format, ...)
{
	if (!m_LogOpen)
		return;

	FormatMsg(Format);
	LogBuffer(ERROR_HEADER);
	AlertBuffer(true);
    if (TInterpreterManager::IsInRuntimeMode())
        CrashNow();
}

void TLogger::Caution(const char *Format, ...)
{
	if (!m_LogOpen)
		return;

	FormatMsg(Format);
	LogBuffer(CAUTION_HEADER);
	if (m_CautionAlert)
		AlertBuffer(false);
}

void TLogger::FatalError(const char *Format, ...)
{
	// We call AlertBuffer before LogBuffer, because
	// the AlertBuffer code is required NOT to
	// call back into FatalError, whereas LogBuffer
	// relies on a lot of subsystems which might
	// somehow fail.
	FormatMsg(Format);
	AlertBuffer(true);
    if (m_LogOpen)
        LogBuffer(FATAL_HEADER);
    CrashNow();
}

void TLogger::CrashNow() {
    PrepareToExit();
    CrashReporter::GetInstance()->CrashNow(m_LogBuffer);
    // We shouldn't get here, but just in case.
	abort();
}

//
//	CheckLog - Check that the log is open, if it isn't and 
//		we haven't failed at opening it once then try to open
//		it again.
//
/*
bool TLogger::CheckLog()
{
	if (m_LogOpen)
		return (true);

	if (m_OpenFailed)
		return (false);

	if (m_FileName.IsEmpty())
		return (false);

	// else try and open it
	if (m_Append)
		m_Log.open(m_FileName.GetString(), std::ios::out | std::ios::app);
	else
		m_Log.open(m_FileName.GetString(), std::ios::out);
	if (m_Log.fail())
		m_OpenFailed = true;
	else
		m_LogOpen = true;

	if (m_LogOpen and m_Append)
	{
		// put welcome line
		m_Log << std::endl;
		TimeStamp();
	}

	return (m_LogOpen);
}
*/

//
//	LogBuffer - 
//
void TLogger::LogBuffer(const char *Header)
{
#ifdef FIVEL_PLATFORM_MACINTOSH
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
#endif // FIVEL_PLATFORM_*

	if (m_LogOpen)
	{
		if (Header != NULL)
			m_Log << Header;

		m_Log << m_LogBuffer << std::endl << std::flush;
	}
}

//
//	AlertBuffer - Display an alert.
//
//  THIS ROUTINE MAY NOT USE 'ASSERT' OR 'FatalError', BECAUSE IS CALLED
//  BY THE ERROR-LOGGING CODE!
//
#ifdef FIVEL_PLATFORM_WIN32

#include <windows.h>

void TLogger::AlertBuffer(bool isError /* = false */)
{
	PrepareToDisplayError();

	uint32 alertType = MB_TASKMODAL | MB_OK;
	if (isError)
		alertType |= MB_ICONSTOP;
	else
		alertType |= MB_ICONINFORMATION;

	::MessageBox(NULL, m_LogBuffer, NULL, alertType);
}

#elif FIVEL_PLATFORM_MACINTOSH

#include <Dialogs.h>
#include <TextUtils.h>

void TLogger::AlertBuffer(bool isError /* = false */)
{
	PrepareToDisplayError();

	// Make sure we don't call StopAlert or CautionAlert before
	// the Toolbox has been initialized.  This typically happens
	// with the Metrowerks SIOUX console when we're running the test
	// suites.
	if (s_ToolboxIsInitialized)
	{
		::CopyCStringToPascal(m_LogBuffer, (StringPtr) m_LogBuffer);
	
		::ParamText((StringPtr) m_LogBuffer, NULL, NULL, NULL);
		if (isError)
    		(void) ::StopAlert(2001, nil);
    	else
    		(void) ::CautionAlert(2001, nil);
    
    	::CopyPascalStringToC((StringPtr) m_LogBuffer, m_LogBuffer);
	}
	else
	{
		std::cerr << std::endl;
		if (isError)
			std::cerr << "ERROR: ";
		else
			std::cerr << "INFO: ";
		std::cerr << m_LogBuffer << std::endl;
	}
}

#elif FIVEL_PLATFORM_OTHER

void TLogger::AlertBuffer(bool isError /* = false */)
{
	PrepareToDisplayError();

	std::cerr << std::endl;
	if (isError)
		std::cerr << "ERROR: ";
	else
		std::cerr << "INFO: ";
	std::cerr << m_LogBuffer << std::endl;
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

	// Initialize the missing media file.
	gMissingMediaLog.Init("MissingMedia", false, true);

	if (inShouldOpenDebugLog)
	{
		// Initialize the debug log.
		gDebugLog.Init("Debug");
		gDebugLog.Log("%s", VERSION_STRING);
	}	
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
void FiveLCheckAssertion(int inTest, const char *inDescription,
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
