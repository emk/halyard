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
// LResource.cpp : 
//

#include "stdafx.h"

#include "TCommon.h" 

#include "LUtil.h"
#include "LResource.h"

#include "Globals.h"
#include "DibLib.h"

#define RES_BUFFER_SPACE	512000	// 500K buffer zone for resource tree

//
//  Add the resource to the priority list. 
//  
LResource::LResource(const char *name) : TBNode(name)
{
    size = 0;
    times_used = 0;
    state = resUnloaded;
}

//  Change the resource's state. If it's unloaded, nothing happens.
//  Otherwise set the state to the new state and update.
//
void LResource::SetState(ResState newState)
{
    if (state > resUnloaded && state != newState) 
        state = newState; 
}

//  We was used! Bump the counter and move the object if necessary.
//  Most of the time calling Load() is enough since that bumps the
//  counter as well, but if there is some aspect of your subclass
//  that counts as a "use" without calling Load(), you can call Used()
//  to bump the counter.
//
void LResource::Used()
{
    times_used++;
}

//  Confirm enough memory is available to load the resource,
//  then load it. Loading a resource counts as a use, so bump
//  use counter as well.
//
void LResource::Load()
{
    if (IsUnloaded()) 
    {	
        _Load();
		state = resLoaded;
    }

    times_used++;
}

//  Purge if loaded; update priority.
//
void LResource::Purge()
{
    if (state > resLocked) 
    {
        _Purge();
        state = resUnloaded;
    }
}


/*  Return true if the other resource should have a higher
    purge priority status than this one.
    
    Determining purge priority status.

    1.  LResource states correspond to initial purge priority.
        (Although resUnloaded and resLocked can't be purged.)

        resUnloaded < resLocked < resLoaded < resPurgeable

    2.  Resources used less than others should be purged first.

    3.  Larger resources should be purged first to make more room.

    4.  If it's still a tie (same state, same use, same size) than
        bugger it and consider the list in order.
*/
int LResource::IsHigher(LResource *other)
{
    if (state != other->state)
        return (other->state > state);

    if (times_used != other->times_used)
        return (other->times_used < times_used);

    if (size != other->size)
        return (other->size > size);
    
    return true;
}
	

//
//	LResourceManager - 
//
LResourceManager::LResourceManager()
{
}

//
//	~LResourceManager
//
LResourceManager::~LResourceManager()
{ 
	RemoveAll();
}

//
//	RemoveAll - Release everything in this resource manager.
//
void LResourceManager::RemoveAll(void)
{
	m_PriorityList.RemoveAll();
	TBTree::RemoveAll();	
}

//
//	AddNode - 
//
void LResourceManager::AddNode(LResource *inRes)
{
	TBTree::Add(inRes);
	m_PriorityList.Add(inRes);

	UpdatePriority(inRes);
}

//  Get the given resource, or return 0 if not found.
//  Every time a resource is requested, consider it used
//  and bump its count.
//
LResource *LResourceManager::GetResource(TString &name)
{
    LResource    *res;

	//
    //  Find may return NULL.
    //
    res = (LResource *) Find(name);

	if (res != NULL)
	{
		res->Used();
		UpdatePriority(res);
	}
    
    return (res);
}

void LResourceManager::RequestMemory(int32 inMemory)
{
	FreeMemory(inMemory);
}

//
//  UpdatePriority - Given the resource pointer, find the index value and
//  	call Reorder.
//
void LResourceManager::UpdatePriority(LResource *res)
{
    int32     theIndex;
    
    theIndex = m_PriorityList.Index(res);
    if (m_PriorityList.ValidIndex(theIndex))
    {
    	Reorder(theIndex);
    	return;
    }
    
    ASSERT(false);
}

//
//  Reorder - Find the given resource in the list and percolate it up
//  	or down depending upon its newly changed priority status.
//
void LResourceManager::Reorder(int32 inIndex)
{
    PriorityDirection	theDir;         //  Direction of movement
    int32				theElement;		// the element that may move
    
    if (m_PriorityList.ValidIndex(inIndex))
    {
    	theElement = inIndex;
    	theDir = dirUp;
    	
    	// see if we have to move it higher in the list
    	while (ShouldMove(theElement, theDir))
    	{
    		Swap(theElement, theElement + 1);
    		theElement++;
    	}
    	
    	// if it moved, stop here
    	if (theElement != inIndex)
    		return;

    	theDir = dirDown;
        
        // see if we have to move it lower in the list
    	while (ShouldMove(theElement, theDir)) 
    	{
        	Swap(theElement, theElement - 1);
        	theElement--;
    	}
    }
}

//
//  ShouldMove - Returns true if the given item should move in the 
//		given direction.
//
bool LResourceManager::ShouldMove(int32 inIndex, PriorityDirection inDir)
{
    LResource	*theLower, *theHigher;
    
    if (m_PriorityList.ValidIndex(inIndex))
    {
    	// check endpoint conditions
    	if ((inDir == dirUp) and (inIndex == (m_PriorityList.NumItems() - 1)))
    		return (false);
    	if ((inDir == dirDown) and (inIndex == 0))
    		return (false);

        if (inDir == dirUp)
        {
        	theLower = (LResource *) m_PriorityList.Item(inIndex);
        	theHigher = (LResource *) m_PriorityList.Item(inIndex + 1);
        }
        else
        {
        	theLower = (LResource *) m_PriorityList.Item(inIndex - 1);
        	theHigher = (LResource *) m_PriorityList.Item(inIndex);
        }
        
        //return (theHigher->IsHigher(theLower));
        return (not (theLower->IsHigher(theHigher)));
    }
   
   	ASSERT(false);
   	return (false);
}

//
//  Swap two items in the array.
//
void LResourceManager::Swap(int32 inIndex1, int32 inIndex2)
{
    LResource	*temp;
    
    if ((m_PriorityList.ValidIndex(inIndex1)) 
		and (m_PriorityList.ValidIndex(inIndex2)))
    {
    	temp = (LResource *) m_PriorityList.Item(inIndex1);
    	m_PriorityList.Set(m_PriorityList.Item(inIndex2), inIndex1);
    	m_PriorityList.Set(temp, inIndex2);
    }
}

//
//	FreeMemory - Make sure we have enough memory to read in
//		inMem bytes.
//
void LResourceManager::FreeMemory(int32 inMem)
{
	LResource	*theRes;
	DWORD		freeSpace;
	DWORD		theMemNeeded = (DWORD) inMem;
	int32		theItem;
	bool		done = false;	
	
	theItem = m_PriorityList.NumItems() - 1;		// start at the end (lowest priority) 
									// and move forward
	MEMORYSTATUS	memStat;

	::GlobalMemoryStatus(&memStat);
	freeSpace = memStat.dwAvailVirtual;

	theMemNeeded += RES_BUFFER_SPACE;	// 400K buffer zone

	if (freeSpace > theMemNeeded)
	{
//		gDebugLog.Log("Loading a resource: have <%ld> free, want <%ld>", freeSpace, theMemNeeded);
		return;
	}
		
	// go through the list freeing resoures until we have
	// enough memory or get to a locked or unloaded resource
	//	
    while ((not done) and (freeSpace < theMemNeeded))
    {
		gDebugLog.Log("Purging resources looking for <%ld> bytes, have <%ld> free", inMem, freeSpace);
    	if (not m_PriorityList.ValidIndex(theItem))
    	{
    		gLog.Log("Out of memory: resource tree full");
    		return;
    	}
    	
    	theRes = (LResource *) m_PriorityList.Item(theItem);
    	
    	switch (theRes->GetState())
    	{
    		case resUnloaded:
    			gLog.Log("Out of memory: all resources purged");
    			done = true;
    			break;
    		case resLocked:
    			gLog.Log("Out of memory: remaining resources locked");
    			done = true;
    			break;
    		default:
    			theRes->Purge();
    			theItem--;

				MEMORYSTATUS	memStat;

				::GlobalMemoryStatus(&memStat);
				freeSpace = memStat.dwAvailVirtual;

    			break;
    	}
    }
}

/*
 $Log$
 Revision 1.1.2.1  2002/03/13 15:06:56  emk
 Merged changed from 3.1.1 -> 3.2.1 into the 3.2.0.1 codebase,
 because we want these in the stable engine.  Highlights:

   1) FiveL.prefs file support.
   2) Removal of -D command line flag.

 Revision 1.2  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  2000/03/01 15:46:55  chuck
 no message

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
