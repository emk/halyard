// -*- Mode: C++; tab-width: 4; -*-
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
// LCommandKey.cpp : 
//

#include "stdafx.h"
#include "LCommandKey.h"
#include "Globals.h"

//
//	LCommandKey - Constructor
//
LCommandKey::LCommandKey(char inKey, TCallback *inCallback)
{
	// do we need to check the key in any way??
	m_Key = inKey;
	m_Callback = inCallback;
}

//
//	LCommandKeyManager - 
//
LCommandKeyManager::LCommandKeyManager()
{

}

//
//	~LCommandKeyManager - 
//
LCommandKeyManager::~LCommandKeyManager()
{
	RemoveAll();
}

//
//	RemoveAll - 
//
void LCommandKeyManager::RemoveAll(void)
{
	DeleteAll();
}

//
//	AddCommandKey - 
//
void LCommandKeyManager::AddCommandKey(char inKey, TCallback *inCallback)
{
	LCommandKey	 *newKey;

	RemoveCommandKey(inKey);
	
	if (inCallback == NULL)
		return;

	newKey = new LCommandKey(inKey, inCallback);
	if (newKey == NULL)
	{
		gLog.Error("Out of memory! You should restart Windows.");
		return;
	}

	Add(newKey);
}

//
//	RemoveCommandKey - 
//
void LCommandKeyManager::RemoveCommandKey(char inKey)
{
	int32	theKeyIndex = -1;

	theKeyIndex = FindCommandKey(inKey);

	if (ValidIndex(theKeyIndex))
		DeleteIndex(theKeyIndex);	// deletes and frees memory
}

//
//	GetCommandKey
//
LCommandKey *LCommandKeyManager::GetCommandKey(char inKey)
{
	LCommandKey		*theKey = NULL;
	int32			theKeyIndex;

	theKeyIndex = FindCommandKey(inKey);
	if (ValidIndex(theKeyIndex))
		theKey = (LCommandKey *) Item(theKeyIndex);

	return (theKey);
}
	
//
//	FindCommandKey
//
int32 LCommandKeyManager::FindCommandKey(char inKey)
{
	LCommandKey		*theKey;
	int32			index;;

	for (index = 0; index < NumItems(); index++)
	{
		theKey = (LCommandKey *) Item(index);

		if (theKey->Equals(inKey))
			// found it
			return (index);
	}

	return (-1);
}

/*
 $Log$
 Revision 1.2  2002/06/20 16:32:55  emk
 Merged the 'FiveL_3_3_4_refactor_lang_1' branch back into the trunk.  This
 branch contained the following enhancements:

   * Most of the communication between the interpreter and the
     engine now goes through the interfaces defined in
     TInterpreter.h and TPrimitive.h.  Among other things, this
     refactoring makes will make it easier to (1) change the interpreter
     from 5L to Scheme and (2) add portable primitives that work
     the same on both platforms.
   * A new system for handling callbacks.

 I also slipped in the following, unrelated enhancements:

   * MacOS X fixes.  Classic Mac5L once again runs under OS X, and
     there is a new, not-yet-ready-for-prime-time Carbonized build.
   * Bug fixes from the "Fix for 3.4" list.

 Revision 1.1.10.1  2002/06/06 05:47:30  emk
 3.3.4.1 - Began refactoring the Win5L interpreter to live behind an
 abstract interface.

   * Strictly limited the files which include Card.h and Macro.h.
   * Added TWin5LInterpreter class.
   * Made as much code as possible use the TInterpreter interface.
   * Fixed a few miscellaneous build warnings.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
