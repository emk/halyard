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
// LRegistry.h : header file for class LRegistry
//

#ifndef __LREGISTRY_H__
#define __LREGISTRY_H__

#include "TString.h"			// header file for class TString

/*-----------------------------------------------------------------

CLASS
    LRegistry

	Class for reading/writing values from/to the Windows Registry.
	Currently we only support reading strings.

AUTHOR
    Yijin He<br>

------------------------------------------------------------------*/

class LRegistry
{
public:
	//////////
	// Default Constructor. 
	//
	LRegistry();

	//////////
	// Deconstructor.
	//
	~LRegistry();

// LRegistry methods
public:
	//////////
	// Empty the string and re-initialize the variables.
	//
	void	ClearKey();

	//////////
	// Set the root key with the specified value.
	//
	// [in] hRootKey - root key value.
	// [out] return - false if hRootKey is not a valid key name, otherwise true.
	//
	bool	SetRootKey(HKEY hRootKey);

	//////////
	// Open or create a specified key and make it current key.
	//
	// [in] strKey - name of the subkey to open/create.
	// [in] bCreate - if we are going to create the key?
	// [out] return - false if unable to create or open the specified key, otherwise true.
	//
	bool	SetKey(TString strKey, bool bCreate);

	//////////
	// Query the type of a specified key.
	//
	// [in] strValueName - name of the key to query.
	// [out] return - the type of the key.
	//
	DWORD	GetDataType(TString strValueName);


	//////////
	// Read the string value from a specified value name associated with a registry key.
	//
	// [in] keyName - name of the key to read.
	// [in/out] str - the string read from the registry will be placed here
	// [out] return - true if success, false otherwise
	//
	bool	ReadString(TString keyName, TString str);

public:

	//////////
	// The error code of the last registry operation.
	//
	int m_nLastError;

protected:
	//////////
	// Handle to the root key.
	//
	HKEY		m_hRootKey;

	//////////
	// If we are lazy to flush the open key?
	//
	//	true:  don't flush.
	//	false: flush all the attributes to the open key more often.
	//
	BOOL		m_bLazyWrite;

	//////////
	// Name of subkey to open.
	//
	TString		m_strCurrentPath;

};	// end of LRegistry class definition


#endif		// __LREGISTRY_H__