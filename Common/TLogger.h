//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, 2000 Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// TLogger.h : 
//

#if !defined (_TLogger_h_)
#define _TLogger_h_

#include <fstream>

#include "TCommon.h"
#include "TObject.h"
#include "TString.h"

#define LOG_NONE		0x00000000
#define LOG_ALL			0xFFFFFFFF

#define LOG_BUFFER_SIZE	10240

/*-----------------------------------------------------------------

CLASS
    TLogger

	A class for logging output to a file.

MACRO DEFINITIONS
	#define LOG_NONE<br>
	#define LOG_ALL<br>
	#define LOG_BUFFER_SIZE<br>

	#define FATAL_HEADER<br>
	#define ERROR_HEADER<br>
	#define CAUTION_HEADER<br>

	#define FormatMsg(Format)<br>

AUTHOR
	Chuck Officer<br>
	Sean Sharp

-----------------------------------------------------------------*/
class TLogger : public TObject
{
public:
	//////////
	// Constructor.
	//
	TLogger();
	
	//////////
	// Constructor.
	//
	~TLogger();

	//////////
	// Initialize the log file.  Assumes path is current directory.
	//
	// [in] Name - name of the log file (no file extension)
	// [in_optional] OpenFile - if true opens the file on init (default true)
	//
	void	Init(const char *Name, bool OpenFile = true);

	//////////
	// Initialize the log file.
	//
	// [in] Path - path where the log file should be written
	// [in] Name - name of the log file (no file extension)
	// [in_optional] OpenFile - if true, opens the file on init (default true)
	// [in_optional] Append - if true, opens file for append only (defualt false)
	//
	void	Init(const char *Path, const char *Name, bool OpenFile = true, bool Append = false);

	//////////
	// Log a general message.
	//
	// [in] Mask - a mask to check against the log mask before logging this message
	// [in] Format - a printf format string (e.g. "Count is %d.", count)
	//
	void	Log(int32 Mask, const char *Format, ...);
	
	//////////
	// Log a general message.
	//
	// [in] Format - a printf format string (e.g. "Count is %d.", count)
	//
	void	Log(const char *Format, ...);	
	
	//////////
	// Log an error message.  Prepends ERROR_HEADER.
	//
	// [in] Format - a printf format string (e.g. "Count is %d.", count)
	//
	void	Error(const char *Format, ...);
	
	//////////
	// Log a caution message.  Prepends CAUTION_HEADER.
	//
	// [in] Format - a printf format string (e.g. "Count is %d.", count)
	//
	void	Caution(const char *Format, ...);
	
	//////////
	// Log a fatal error message.  Prepends FATAL_HEADER and calls Shutdown().
	//
	// [in] Format - a printf format string (e.g. "Count is %d.", count)
	//
	void	FatalError(const char *Format, ...);
	
	//////////
	// Put a time stamp in the log.
	//
	void	TimeStamp(void);

	//////////
	// Turn all logging off.
	//
	inline void LogNothing(void) { m_LogMask = LOG_NONE; }
	
	//////////
	// Logical "or" the log mask with the given mask.
	//
	// [in] Mask - the mask
	//
	inline void AddLogMask(int32 Mask) { m_LogMask |= Mask; }
	
	//////////
	// Turn all loggin on.
	//
	inline void LogEverything(void) { m_LogMask = LOG_ALL; }

private:
	//////////
	// File output stream
	//
	std::ofstream	m_Log;
	
	//////////
	// Filename for log output
	//
	TString		m_FileName;
	
	//////////
	// Log buffer
	//
	char		m_LogBuffer[LOG_BUFFER_SIZE];
	
	//////////
	// Has the log file been opened for writing?
	//
	bool		m_LogOpen;
	
	//////////
	// Was there an error when opening the log file for writing?
	//
	bool		m_OpenFailed;
	
	//////////
	// If true, an alert message box is displayed for caution messages in 
	// addition to logging the message.
	//
	bool		m_CautionAlert;
	
	//////////
	// Is the log output in append mode?
	//
	bool		m_Append;
	
	//////////
	// Logger mask
	//
	uint32		m_LogMask;

	// Deprecated
	//
	//bool		CheckLog();
	
	//////////
	// Write the contents of m_LogBuffer to the log file.
	//
	// [in] Header - a header to precede the log buffer contents
	//
	void		LogBuffer(const char *Header);
	
	//////////
	// Display an alert message box with the contents of m_LogBuffer.
	//
	// [in] inError - Is it an error message?
	//
	void		AlertBuffer(bool isError = false);

	//////////
	// Should a log message with a given mask be logged?
	//
	// [in] Mask - a mask to check against
	// [out] return - true if the message should be logged, false otherwise
	//
	inline bool	ShouldLog(int32 Mask) 
	{ 
		if (m_LogOpen)
			if ((m_LogMask & Mask) != 0)
				return (true);
		return (false);
	}
};

#endif // _TLogger_h_

/*
 $Log$
 Revision 1.2  2002/02/19 12:35:11  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.1  2000/04/06 17:06:10  chuck
 Initial check-in

*/
