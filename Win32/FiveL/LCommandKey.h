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
#include "Card.h"
//class Card;

/*-----------------------------------------------------------------

CLASS
    LCommandKey

	A keybinding class that attaches a Card to be called when a certain 
	keystroke is made.  Associated control keys should be handled at a higher level.   

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
		// [in] inCard - the Card to jump to when keybinding is invoked
		//
		LCommandKey(char inKey, Card *inCard);
		
		//////////
		// Destructor.
		//
		~LCommandKey() {}

		//////////
		// Get the key for this binding.
		//
		// [out] return - the key for this binding
		//
		inline char	GetKey(void) { return (m_Key); }

		//////////
		// Get the Card for this binding.
		//
		// [out] return - the Card for this binding
		//
		inline Card	*GetCard(void) { return (m_Card); }
		
		//////////
		// Get the name of the Card for this binding.
		//
		// [out] return - the name of the Card
		//
		inline const char *GetCardName(void) { return (m_CardName.GetString()); }

		//////////
		// Compare the key for this binding to another key.
		//
		// [in] inKey - key for comparison
		// [out] return - true if the key are equal, false otherwise
		//
		inline bool	Equals(char inKey) { return ((m_Key == inKey) ? true : false); }

	protected:
		char		m_Key;
		TString		m_CardName;	// needed to rebuild key bindings
		Card		*m_Card;
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
		// [in] inCard - the Card to jump to when keybinding is invoked
		//
		void		AddCommandKey(char inKey, Card *inCard);
		
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
		
		//////////
		// Rebuild key bindings.  This should be run when Card pointers become
		// invalid (e.g. after a RedoScript)
		//
		void		RebuildKeyBindings();

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
