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
// KLogger.h : MacOS version
//

#if !defined (_KLogger_h_)
#define _KLogger_h_

#include <fstream>

#include "TCommon.h"
#include "TObject.h"
#include "TString.h"

BEGIN_NAMESPACE_FIVEL

#define LOG_NONE		0x00000000
#define LOG_ALL			0xFFFFFFFF

#define LOG_BUFFER_SIZE	10240

class KLogger : public TObject
{
public:
			KLogger();
			~KLogger();

	void	Init(const char *Name, bool OpenFile = true);
	void	Init(const char *Path, const char *Name, bool OpenFile = true, bool Append = false);

	void	Log(int32 Mask, const char *Format, ...);
	void	Log(const char *Format, ...);	
	void	Error(const char *Format, ...);
	void	Caution(const char *Format, ...);
	void	FatalError(const char *Format, ...);
	void	TimeStamp(void);

	inline void LogNothing(void) { m_LogMask = LOG_NONE; }
	inline void AddLogMask(int32 Mask) { m_LogMask |= Mask; }
	inline void LogEverything(void) { m_LogMask = LOG_ALL; }

protected:
	std::ofstream	m_Log;
	TString		m_FileName;
	char		m_LogBuffer[LOG_BUFFER_SIZE];
	bool		m_LogOpen;
	bool		m_OpenFailed;
	bool		m_CautionAlert;
	bool		m_Append;
	uint32		m_LogMask;

	bool		CheckLog();
	void		LogBuffer(const char *Header);
	void		AlertBuffer(bool isError = false);
#ifdef MACOS
	void		XFormBuffer(void);
#endif

	inline bool	ShouldLog(int32 Mask) 
	{ 
		if (m_LogOpen)
			if ((m_LogMask & Mask) != 0)
				return (true);
		return (false);
	}
};

END_NAMESPACE_FIVEL

#endif // _KLogger_h_

/*
 $Log$
 Revision 1.2  2002/03/04 17:12:44  hamon
 Added support for compiler's namespaces.

Made changes to reflect change in common files names (i.e. references to KString, KRect, etc changed to TString, TRect, etc).

Changes by Elizabeth, okayed by Eric.

 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
