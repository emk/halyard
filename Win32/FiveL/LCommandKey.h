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
// LCommandKey.h : 
//

#if !defined (_LCommandKey_h_)
#define _LCommandKey_h_


#include "TCommon.h"
#include "TObject.h"
#include "TArray.h"
#include "TString.h"
#include "TInterpreter.h"

/*-----------------------------------------------------------------

CLASS
    LCommandKey

	A keybinding class that attaches a callback to be called when a certain
	keystroke is made.  Associated control keys should be handled at a
	higher level.

AUTHOR
    Chuck Officer<br>
	Sean Sharp

-----------------------------------------------------------------*/
class LCommandKey : public TObject
{
	public:
		//////////
		// Constructor.  Creates a new keybinding.
		//
		// [in] inKey - the key
		// [in] inCallback - the callback to run when keybinding is invoked.
		// We assume ownership of this object, and delete it when we're done.
		//
		LCommandKey(char inKey, TCallback *inCallback);
		
		//////////
		// Destructor.
		//
		~LCommandKey() { delete m_Callback; }

		//////////
		// Get the key for this binding.
		//
		// [out] return - the key for this binding
		//
		inline char	GetKey(void) { return (m_Key); }

		//////////
		// Get the callback for this binding.
		//
		// [out] return - the callback for this binding
		//
		inline TCallback *GetCallback(void) { return (m_Callback); }
		
		//////////
		// Compare the key for this binding to another key.
		//
		// [in] inKey - key for comparison
		// [out] return - true if the key are equal, false otherwise
		//
		inline bool	Equals(char inKey)
			{ return ((m_Key == inKey) ? true : false); }

	protected:
		char		m_Key;
		TCallback	*m_Callback;
};

/*-----------------------------------------------------------------

CLASS
    LCommandKeyManager

	Manages a set of LCommandKey keybindings.  A seperate 
	LCommandKeyManager instance should be used for each control key
	(i.e. one for ALT-KEY sequences and another for CTRL-KEY).

AUTHOR
    Chuck Officer<br>
	Sean Sharp

-----------------------------------------------------------------*/
class LCommandKeyManager : protected TArray
{
	public:
		//////////
		// Constructor.
		//
		LCommandKeyManager();

		//////////
		// Destructor.  Calls RemoveAll().
		//
		~LCommandKeyManager();

		//////////
		// Deletes all keybindings from our list.
		//
		void		RemoveAll(void);

		//////////
		// Add an LCommandKey binding to the list.
		//
		// [in] inKey - the key 
		// [in] inCallback - the callback to call when keybinding is invoked.
		// We assume ownership of this object, and delete it when we're done.
		//
		void		AddCommandKey(char inKey, TCallback *inCallback);
		
		//////////
		// Remove a key binding from the list.
		//
		// [in] inKey - the key 
		//
		void		RemoveCommandKey(char inKey);
		
		//////////
		// Get LCommandKey object for a key.
		//
		// [in] inKey - the key
		// [out] return - a pointer to an LCommandKey object
		//
		LCommandKey	*GetCommandKey(char inKey);
		
	protected:
		//////////
		// Find the index for the LCommandKey associated with the given key.
		//
		// [in] inKey - the key
		// [out] return - the index
		//
		int32		FindCommandKey(char inKey);
};

#endif // _LCommandKey_h_

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

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
