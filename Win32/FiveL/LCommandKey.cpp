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
LCommandKey::LCommandKey(char inKey, Card *inCard)
{
	// do we need to check the key or card in any way??
	m_Key = inKey;
	m_Card = inCard;
	m_CardName = inCard->Name();
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
void LCommandKeyManager::AddCommandKey(char inKey, Card *inCard)
{
	LCommandKey	 *newKey;

	RemoveCommandKey(inKey);
	
	if (inCard == NULL)
		return;

	newKey = new LCommandKey(inKey, inCard);
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

void LCommandKeyManager::RebuildKeyBindings()
{

	LCommandKey		*oldKey;
	int32			index;

	for (index = 0; index < NumItems(); index++)
	{	
		oldKey = (LCommandKey *) Item(0);
		
		// Will delete old key binding and make a new one 
		AddCommandKey(oldKey->GetKey(), gCardManager.GetCard(oldKey->GetCardName())); 
	}
}

/*
 $Log$
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
