//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// KLogger.cpp : 
//

#include "THeader.h"

#include "KLogger.h"
#include "TString.h"

#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <time.h>

#ifdef MACOS
#include "MacUtils.h"

USING_NAMESPACE_FIVEL

#ifdef DEBUG
	const long	FILE_CREATOR = FOUR_CHAR_CODE('R*ch');
#else
	const long	FILE_CREATOR = FOUR_CHAR_CODE('ttxt');
#endif // DEBUG
	const long	FILE_TYPE = FOUR_CHAR_CODE('TEXT');
	
#endif	// MACOS

#define FATAL_HEADER	"Fatal Error: "
#define ERROR_HEADER	"Error: "
#define CAUTION_HEADER	"Caution: "

#ifdef WIN32
#define FormatMsg(Format)	\
	va_list	argPtr;			\
	va_start(argPtr, (Format));		\
	wvsprintf(m_LogBuffer, (Format), argPtr);	\
	va_end(argPtr)
#endif
#ifdef MACOS
#define FormatMsg(Format)	\
	va_list	argPtr;			\
	va_start(argPtr, (Format));		\
	vsprintf(m_LogBuffer, (Format), argPtr);	\
	va_end(argPtr)
#endif

KLogger::KLogger()
{ 
	m_LogOpen = false; 
	m_OpenFailed = false;
	m_CautionAlert = false; 
	m_Append = false;
	m_LogMask = LOG_ALL;
}

KLogger::~KLogger()
{
	if (m_LogOpen)
		m_Log.close();
}

//
//	Init - Initialize the log file. 
//
void KLogger::Init(const char *Path, const char *Name, 
				   bool OpenFile /* = true */, bool Append /* = false */)
{
	m_Append = Append;

	m_FileName = Path;
	
#ifdef WIN32
	if (not m_FileName.EndsWith('\\'))
		m_FileName += '\\';
#endif
#ifdef MACOS
	if (not m_FileName.EndsWith(':'))
		m_FileName += ':';
#endif

	m_FileName += Name;
	m_FileName += ".log";
		
	if (OpenFile)
	{
#ifdef MACOS
		// MacOS - turn the path into a FSSpec
		FSSpec		theSpec;
		if (PathToFSSpec(m_FileName, &theSpec))
		{
			// Make sure the file exists so that we can set the creator and type.
			::FSpCreateResFile(&theSpec, FILE_CREATOR, FILE_TYPE, smRoman);
		}
#endif

		if (m_Append)
			m_Log.open(m_FileName.GetString(), std::ios::out | std::ios::app);
		else
			m_Log.open(m_FileName.GetString(), std::ios::out);

		if (not m_Log.fail())
			m_LogOpen = true;
		else
			m_OpenFailed = true;

		if (m_LogOpen and m_Append)
		{
			// put welcome line
			m_Log << std::endl;
			TimeStamp();
		}
	}
}

//
//	Init - Initialize the log file. Use the current directory.
//
void KLogger::Init(const char *Name, bool OpenFile /* = true */)
{	
#ifdef WIN32
	char		pathBuf[MAX_PATH];
	DWORD		retLen = 0;
	
	retLen = ::GetCurrentDirectory(MAX_PATH, pathBuf);
	if (retLen > 0)
		Init(pathBuf, Name, OpenFile);
#endif
#ifdef MACOS
	TString		path = ":";		// start from current directory by default
	
	Init(path.GetString(), Name, OpenFile);
#endif	
}

void KLogger::Log(int32 Mask, const char *Format, ...)
{
	if (not CheckLog())
		return;

	if (not ShouldLog(Mask))
		return;

	FormatMsg(Format);
#ifdef MACOS
	XFormBuffer();
#endif
	LogBuffer(NULL);	
}

void KLogger::Log(const char *Format, ...)
{
	if (not CheckLog())
		return;

	FormatMsg(Format);
#ifdef MACOS
	XFormBuffer();
#endif
	LogBuffer(NULL);
}

void KLogger::Error(const char *Format, ...)
{
	if (not CheckLog())
		return;

	FormatMsg(Format);
#ifdef MACOS
	XFormBuffer();
#endif
	LogBuffer(ERROR_HEADER);
	AlertBuffer(true);
}

void KLogger::Caution(const char *Format, ...)
{
	if (not CheckLog())
		return;

	FormatMsg(Format);
#ifdef MACOS
	XFormBuffer();
#endif
	LogBuffer(CAUTION_HEADER);
	if (m_CautionAlert)
		AlertBuffer(false);
}

void KLogger::FatalError(const char *Format, ...)
{
	if (not CheckLog())
		return;

	FormatMsg(Format);
#ifdef MACOS
	XFormBuffer();
#endif
	LogBuffer(FATAL_HEADER);
	AlertBuffer(true);
//	ShutDown(true);
}

//
//	CheckLog - Check that the log is open, if it isn't and 
//		we haven't failed at opening it once then try to open
//		it again.
//
bool KLogger::CheckLog()
{
	if (m_LogOpen)
		return (true);
	if (m_OpenFailed)
		return (false);

	// else try and open it
	
#ifdef MACOS
	// MacOS - turn the path into a FSSpec
	FSSpec		theSpec;
	if (PathToFSSpec(m_FileName, &theSpec))
	{
		// Make sure the file exists so that we can set the creator and type.
		::FSpCreateResFile(&theSpec, FILE_CREATOR, FILE_TYPE, smRoman);
	}
#endif

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

//
//	LogBuffer - 
//
void KLogger::LogBuffer(const char *Header)
{
	if (m_LogOpen)
	{
		if (Header != NULL)
			m_Log << Header;

		m_Log << m_LogBuffer << std::endl;
	}
}

//
//	AlertBuffer -
//
void KLogger::AlertBuffer(bool isError /* = false */)
{
#ifdef WIN32
	uint32		alertType;

	alertType = MB_SYSTEMMODAL | MB_OK;
	if (isError)
		alertType |= MB_ICONSTOP;
	else
		alertType |= MB_ICONINFORMATION;

	::MessageBox(::GetFocus(), m_LogBuffer, NULL, alertType);
#else
	c2pstr(m_LogBuffer);
	
	::ParamText((const uint8 *) m_LogBuffer, NULL, NULL, NULL);
	if (isError)
    	(void) ::StopAlert(2001, nil);
    else
    	(void) ::CautionAlert(2001, nil);
    
    p2cstr((unsigned char *) m_LogBuffer);
#endif
}

//
//	TimeStamp - Put a time stamp in the log
//
void KLogger::TimeStamp(void)
{
	time_t	timeNow;
	char	*timeStrPtr;

	time(&timeNow);
	timeStrPtr = ctime(&timeNow);
	m_Log << timeStrPtr << std::endl;
}

#ifdef MACOS
//
//	XFormBuffer - Do Mac only transformations.
//
void KLogger::XFormBuffer(void)
{
	int	logLen = strlen(m_LogBuffer);
	
	for (int i = 0; i < logLen; i++)
	{
		if (m_LogBuffer[i] == NEWLINE_CHAR)
			m_LogBuffer[i] = RETURN_CHAR;
	}
}
#endif


/*
 $Log$
 Revision 1.2  2002/03/04 17:12:43  hamon
 Added support for compiler's namespaces.

Made changes to reflect change in common files names (i.e. references to KString, KRect, etc changed to TString, TRect, etc).

Changes by Elizabeth, okayed by Eric.

 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
