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
// LRegistry.cpp : implementation file of class LRegistry
//

#include "stdafx.h"
#include <winreg.h>
#include "LRegistry.h"

#define CLASS_NAME_LENGTH 255

// IMPORTANT NOTES ABOUT LREGISTRY:
//	
//	LRegistry never keeps a key open past the end of a function call.
//	This is incase the application crashes before the next call to close
//	the registry 
//	
//	INCLUDE FILES
//	"winreg.h" and "afxdisp.h" must be included in "stdafx.h"
//
//	KEY NAMES:
//	Key names must not begin with a \ and only absolute strings are accepted


//
//	LRegistry - constructor
//
LRegistry::LRegistry()
{
	m_hRootKey		= HKEY_CURRENT_USER;
	m_bLazyWrite	= TRUE;
	m_nLastError	= ERROR_SUCCESS;
}

//
//	~LRegistry - deconstructor
//
LRegistry::~LRegistry()
{
	ClearKey();
}

//
//	ClearKey - 	empty the string and reset the variables
//
void LRegistry::ClearKey()
{
	m_strCurrentPath.Empty();
	m_hRootKey		= HKEY_CURRENT_USER;
	m_bLazyWrite	= TRUE;
}

//
//	GetDataType - retrieve the type of a give value name associated with a registry key
//
DWORD LRegistry::GetDataType(TString strValueName)
{
	HKEY hKey;

	m_nLastError = ::RegOpenKeyEx(m_hRootKey, LPCTSTR(m_strCurrentPath), 0, KEY_QUERY_VALUE, &hKey);

	if (m_nLastError != ERROR_SUCCESS) 
		return 0;

	DWORD dwType = 1;
	m_nLastError = ::RegQueryValueEx(hKey, LPCTSTR(strValueName), NULL, &dwType, NULL, NULL);
	::RegCloseKey(hKey);		

	if (m_nLastError == ERROR_SUCCESS) 
		return dwType;

	return 0;
}


//
//	SetRootKey - set the root key
//
bool LRegistry::SetRootKey(HKEY hRootKey)
{
	// sets the root key
	// make sure to set it to a valid key
	if (hRootKey != HKEY_CLASSES_ROOT &&
		hRootKey != HKEY_CURRENT_USER &&
		hRootKey != HKEY_LOCAL_MACHINE &&
		hRootKey != HKEY_USERS) 
			return FALSE;

	m_hRootKey = hRootKey;
	return true;
}


//
//	SetKey - opens a specified key and make it current key
//
bool LRegistry::SetKey(TString strKey, bool bCreate)
{
	//	Call SetKey to make a specified key the current key. Key is the 
	//	name of the key to open. If Key is null, the CurrentKey property
	//	is set to the key specified by the RootKey property.
	//
	//	CanCreate specifies whether to create the specified key if it does 
	//	not exist. If CanCreate is True, the key is created if necessary.
	//
	//	Key is opened or created with the security access value KEY_ALL_ACCESS. 
	//	OpenKey only creates non-volatile keys, A non-volatile key is stored in 
	//	the registry and is preserved when the system is restarted. 
	//
	//	OpenKey returns True if the key is successfully opened or created

	HKEY hKey;

	// close the current key if it is open
	if (strKey.Length() == 0)
	{
		m_strCurrentPath.Empty();
		return false;
	}

	DWORD dwDisposition;
	if (bCreate)		// open the key with RegCreateKeyEx
	{
		if (::RegCreateKeyEx(m_hRootKey, LPCTSTR(strKey), 0, NULL, 
			REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, &hKey,
			&dwDisposition) != ERROR_SUCCESS) 
				return FALSE;

		m_strCurrentPath = strKey;
		
		if (!m_bLazyWrite) 
			::RegFlushKey(hKey);
		
		::RegCloseKey(hKey);	
		return true;
	}

	// otherwise, open the key without creating
	// open key requires no initial slash
	m_nLastError = ::RegOpenKeyEx(m_hRootKey, LPCTSTR(strKey), 0, KEY_ALL_ACCESS, &hKey);
	
	if (m_nLastError != ERROR_SUCCESS) 
		return false;
	
	m_strCurrentPath = strKey;
	
	if (!m_bLazyWrite) 
		::RegFlushKey(hKey);
	
	::RegCloseKey(hKey);

	return true;
}

//
//	ReadString - read the string value from a specified value name associated with a registry key
//
bool LRegistry::ReadString(TString keyName, TString &str)
{
	DWORD dwType = REG_SZ;
	DWORD dwSize = 255;
	BOOL bSuccess = TRUE;
	_TCHAR sz[255];
	HKEY hKey;

	// make sure it is the proper type
	dwType = GetDataType(keyName);
	
	if (dwType != REG_SZ && dwType != REG_EXPAND_SZ)
		return false;

	m_nLastError = ::RegOpenKeyEx(m_hRootKey, LPCTSTR(m_strCurrentPath), 0, KEY_READ, &hKey);
	
	if (m_nLastError != ERROR_SUCCESS) 
		return false;

	m_nLastError = ::RegQueryValueEx(hKey, LPCTSTR(keyName), NULL, &dwType, (LPBYTE)sz, &dwSize);
	
	if (m_nLastError != ERROR_SUCCESS) 
		bSuccess = FALSE;
	
	::RegCloseKey(hKey);	
	
	if (!bSuccess) 
		return false;

	str = TString((LPCTSTR)sz);
	return true;
}


// End of file: LRegistry.CPP