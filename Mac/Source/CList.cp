//
//	CList.cp - Handles the priority list of resources. Should really be a part of
//		a resource manager class.
//

#include "debug.h"

#include "CList.h"
#include "util.h"

#define LOW_MEMORY_POINT	518200		// keep 500K when get GrowZone function
#define RES_BUFFER_SPACE	512000		// keep 500K around to keep things moving

/******************************

    PRIORITY LIST ROUTINES

******************************/

CList::CList() : CArray()
{
}

CList::~CList()
{
	RemoveAll();
}

//  Given the resource pointer, find the index value and
//  call Reorder.
//
void CList::Update(CResource *res)
{    
    int32     theIndex;

	theIndex = Index(res);
	if (ValidIndex(theIndex))
	{
		Reorder(theIndex);
		return;
	}
    ASSERT(FALSE)
}

//
//  Reorder - Find the given resource in the list and percolate it up
//  		or down depending upon its newly changed priority status.
//
void CList::Reorder(int32 inIndex)
{
	PDir		theDir;
	int32		theElement;
	
	if (ValidIndex(inIndex))
	{
		theElement = inIndex;
		
		// see if we have to move it higher in the list
		theDir = dirUp;
		while (ShouldMove(theElement, theDir))
		{
			Swap(theElement, theElement + 1);
			theElement++;
		}
		
		// if it moved, stop here
		if (theElement != inIndex)
			return;
			
		// see if we have to move it lower
		theDir = dirDown;
		while (ShouldMove(theElement, theDir))
		{
			Swap(theElement, theElement - 1);
			theElement--;
		}
	}
}

//
//  ShouldMove - Returns TRUE if the given item should move in the given
//  		direction.
//
bool CList::ShouldMove(int32 inIndex, PDir inDir)
{ 
	CResource	*theLower, *theHigher;

	if (ValidIndex(inIndex))
	{
		// check endpoint conditions
		if ((inDir == dirUp) and (inIndex == (NumItems() - 1)))
			return (FALSE);
		if ((inDir == dirDown) and (inIndex == 0))
			return (FALSE);
		
		if (inDir == dirUp)
		{
			theLower = (CResource *) Item(inIndex);
			theHigher = (CResource *) Item(inIndex + 1);
		}
		else
		{
			theLower = (CResource *) Item(inIndex - 1);
			theHigher = (CResource *) Item(inIndex);
		}
		
		return (not (theLower->IsHigher(theHigher)));
	}
	
	ASSERT(false);
	return(false);
}

//
//  Swap two items in the array.
//
void CList::Swap(int32 inIndex1, int32 inIndex2)
{
	CResource		*temp;

	if ((ValidIndex(inIndex1)) and (ValidIndex(inIndex2)))
	{
    	temp = (CResource *) Item(inIndex1);
    	Set(Item(inIndex2), inIndex1);
    	Set(temp, inIndex2);
    }
}

//  Ensure that the necessary memory will be available by
//  purging resources as needed. If we can't free up the
//  needed memory fail.
//
void CList::FreeMemory(int32 inMem)
{
	CResource	*theRes;
	int32		freeMem;
	int32		askForMem;			// how much mem should we ask for
	int32		theIndex;
	bool		done = false;
	
	theIndex = NumItems() - 1;		// start at the end (lowest priority) and move forward
	
	freeMem = ::MaxBlock();
	//freeMem = ::FreeMem();
	askForMem = inMem + RES_BUFFER_SPACE;	// plus a 400K buffer zone

	if (freeMem > askForMem)
	{
#ifdef DEBUG_5L
		//prinfo("Loading a resource, have <%ld> free, only need <%ld>", freeMem, inMem);
#endif
		return;
	}
	else
	{
		freeMem = ::CompactMem(askForMem);
#ifdef DEBUG_5L
		//prinfo("After CompactMem(), <%ld> contiguous bytes free", freeMem);
#endif
	}
		
	while ((not done) and (freeMem < askForMem))
	{
#ifdef DEBUG_5L
		//prinfo("Purging resources looking for <%ld> bytes, have <%ld> free", inMem, freeMem);
#endif

		if (not ValidIndex(theIndex))
		{
			prerror("Out of memory: resource tree full");
			return;
		}
		
		theRes = (CResource *) Item(theIndex);
		
		switch (theRes->GetState())
		{
			case kResUnloaded:
				prerror("Out of memory: all resources purged");
				done = true;
				break;
			case kResLocked:
				prerror("Out of memory: remaining resources locked");
				done = true;
				break;
			default:
				theRes->Purge();
				theIndex--;			// look at the next one down
				freeMem = ::MaxBlock();
				break;
		}
	}
}

//
//	TossMemory - Called when we are getting low on memory. 
//
int32 CList::FreeUpMemory(int32 bytesNeeded)
{
	CResource		*theRes;
	int32			askForMem;
	int32			freeMem;
	int32			initMem;
	int32			theIndex;
	
	theIndex = NumItems() - 1;
	
	initMem = ::MaxBlock();
	freeMem = initMem;
	
	// ask for at least RES_BUFFER_SPACE to keep this from getting called all the time
	askForMem = (bytesNeeded > RES_BUFFER_SPACE) ? bytesNeeded : RES_BUFFER_SPACE;
	
	while (freeMem < askForMem)
	{
		if (not ValidIndex(theIndex))
			return(freeMem - initMem);					// can't do much more
		
		theRes = (CResource *) Item(theIndex);
		
		// stop when we get to a locked or unloaded resource
		if (theRes->GetState() 	<= kResLocked)
			return (freeMem - initMem);
			
		theRes->Purge();	
		theIndex--;
		freeMem = ::MaxBlock();
	}
	
	return (freeMem - initMem);
}
