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
// SingleInstance.h : Class to limit the application to single instance
//

#ifndef __SingleInstance_H__
#define __SingleInstance_H__

#include <windows.h>

class SingleInstance
{
	//////////
	// Constructor.
	//
public:
	SingleInstance(TCHAR *strMutexName)
	{
		// Be sure to use a name that is unique for this application otherwise
		// two apps may think they are the same if they are using same name for
		// 3rd parm to CreateMutex
		m_hMutex = CreateMutex(NULL, FALSE, strMutexName);	// do early
		m_dwLastError = GetLastError();						// save for use later...
	}

	//////////
	// Destructor
	//
	~SingleInstance() 
	{
		if (m_hMutex)				// don't forget to close handles...
		{
			CloseHandle(m_hMutex);	// do as late as possible
			m_hMutex = NULL;		// good habit to be in
		}
	}

	//////////
	// Is there another instance running?
	//
	// [out] return - true if there is another instance running, otherwise false
	//
	BOOL IsAnotherInstanceRunning() 
	{
		return (m_dwLastError == ERROR_ALREADY_EXISTS);
	}

protected:
	//////////
	// Error # of last error.
	//
	DWORD  m_dwLastError;

	//////////
	// Handle for mutex.
	//
	HANDLE m_hMutex;
};


#endif		// __SingleInstance_H__