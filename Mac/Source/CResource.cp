//
//	CResource.cp - Implementation of CResource class.
//

#include "THeader.h"

#include "KLogger.h"

#include "CMac5LApp.h"
#include "CResource.h"

USING_NAMESPACE_FIVEL

//CResourceManager	gResManager;

/*********************

    RESOURCE CLASS

*********************/

static const int 	MAX_MEM_SIZE =   1024000;		// 1 MB of total cache space
static const int	CHUNK_MEM_SIZE =  256000;	// 256K chunk size
  
CResource::CResource(TString &inName) : TBNode(inName)
{
    m_Size = 0;
    m_TimesUsed = 0;
    m_State = kResUnloaded;
}

//
//	Lock - If inLock is true and the resource is in memory, lock it
//		there. If inLock is false and the resource is in memory, make
//		sure it isn't locked.
//
void CResource::Lock(bool inLock)
{
	if (IsLoaded())		// make sure it is in memory first
	{
// cbo_mem - only allow locking of resources if we are managing memory in 
//		the resource tree
		if (inLock)
			SetState(kResLocked);	// lock it
		else
			SetState(kResLoaded);	// unlock it
	}
}

//  Change the resource's state. If it's unloaded, nothing happens.
//  Otherwise set the state to the new state and update.
//
void CResource::SetState(ResState newState)
{
    if ((IsLoaded()) and (m_State != newState))
    {
        m_State = newState;
       	UpdatePriority();
    }
}

//
//	SetSize - Set the size of the resource.
//
void CResource::SetSize(uint32 newSize)
{
	m_Size = newSize;
}

//  We was used! Bump the counter and move the object if necessary.
//  Most of the time calling Load() is enough since that bumps the
//  counter as well, but if there is some aspect of your subclass
//  that counts as a "use" without calling Load(), you can call Used()
//  to bump the counter.
//
void CResource::Used()
{
    m_TimesUsed++;
    UpdatePriority();
}

//  Confirm enough memory is available to load the resource,
//  then load it. Loading a resource counts as a use, so bump
//  use counter as well.
//
void CResource::Load(void)
{
    if (IsUnloaded()) 
    {
#ifdef DEBUG
	//	gDebugLog.Log("loading resource <%s>, size <%ld>", key.GetString(), m_Size);
#endif
    	
        _Load();        	
        m_State = kResLoaded;
   	}
	else
	{
		Used();
	}
}

//  Purge if loaded; update priority.
//
void CResource::Purge()
{
    if (m_State > kResLocked) 
    {
        _Purge();
        m_State = kResUnloaded;
        
        UpdatePriority();
	}
}

/*  Return TRUE if the other resource should have a higher
    purge priority status than this one.
    
    Determining purge priority status.

    1.  Resource states correspond to initial purge priority.
        (Although kResUnloaded and kResLocked can't be purged.)

        kResUnloaded < kResLocked < kResLoaded < kResPurgeable

    2.  Resources used less than others should be purged first.

    3.  Larger resources should be purged first to make more room.

    4.  If it's still a tie (same state, same use, same size) than
        bugger it and consider the list in order.
*/
int16 CResource::IsHigher(CResource *other)
{
    if (m_State != other->m_State)
        return (other->m_State > m_State);

    if (m_TimesUsed != other->m_TimesUsed)
        return (other->m_TimesUsed < m_TimesUsed);

    if (m_Size != other->m_Size)
        return (other->m_Size > m_Size);
    
    return (TRUE);
}

//
// CResourceManager functions
//
//  Get the given resource, or return 0 if not found.
//  Every time a resource is requested, consider it used
//  and bump its count.
//
CResourceManager::CResourceManager()
{
	m_TotalSize = 0;
	m_CheckingMemory = false;
}

CResourceManager::~CResourceManager()
{
	RemoveAll();
}

CResource *CResourceManager::GetResource(TString &inName)
{
    CResource    *res = NULL;

    res = (CResource *) Find(inName.GetString());
    return res;
}

void CResourceManager::AddResource(CResource *newRes)
{
	TBTree::Add(newRes);
	m_PriorityList.Add(newRes);
	
	CheckMemory();
}

void CResourceManager::RemoveAll(void)
{
	m_CheckingMemory = true;
	TBTree::RemoveAll();
	m_PriorityList.RemoveAll();
	m_CheckingMemory = false;
	
	m_TotalSize = 0;
}

void CResourceManager::ChangeResSize(int32 newSize, int32 oldSize)
{
	m_TotalSize -= oldSize;
	m_TotalSize += newSize;

#ifdef DEBUG	
	// cbo_fix - just for debugging purposes
	//gDebugLog.Log("CResourceManager::ChangeResSize: total resource cache size <%d> bytes", m_TotalSize);
#endif
}

void CResourceManager::CheckMemory(void)
{
	int32		maxMemSize = MAX_MEM_SIZE;
	int32		maxChunkSize = CHUNK_MEM_SIZE;
	
	// don't recurse or rearrange the list of resources
	if (not m_CheckingMemory)
	{			
		m_CheckingMemory = true;
		if (m_TotalSize >= maxMemSize)
		{
			// free up some memory
			int32	memToFree = m_TotalSize - maxMemSize;
			memToFree += maxChunkSize;
			FreeMemory(memToFree);
		}
		m_CheckingMemory = false;
	}
}

void CResourceManager::FreeMemory(int32 freeMemSize)
{
	TArray		tmpList;
	CResource	*theRes = NULL;
	int32		resSize = 0;
	int32		totalFreed = 0;
	int32		curIndex;
	bool		done = false;

	// first make a copy of our resource list as the real one
	// will get reordered as resources are purged
	for (int i = 0; i < m_PriorityList.NumItems(); i++)
	{
		if (m_PriorityList.ValidIndex(i))
		{
			tmpList.Add(m_PriorityList.Item(i));
		}
	}
	
	curIndex = tmpList.NumItems() - 1;	// start at the end
		
	while ((not done) and (totalFreed < freeMemSize))
	{
		if (not tmpList.ValidIndex(curIndex))
		{
			// no more memory to free
			gLog.Error("Out of memory: resource tree full");
			return;
		}
		
		theRes = (CResource *) tmpList.Item(curIndex);
		
		switch (theRes->GetState())
		{
			case kResUnloaded:
				gLog.Error("Out of memory: all resources purged");
				done = true;
				break;
			case kResLocked:
				gLog.Error("Out of memory: remaining resources locked");
				done = true;
				break;
			default:
				resSize = theRes->GetSize();
				theRes->Purge();
				totalFreed += resSize;
				curIndex--;			// look at the next one down
				break;
		}
	}

#ifdef DEBUG	
	// if we get here, we have freed the memory
	// cbo_fix - just for debugging purposes
	//gDebugLog.Log("CResourceManager::FreeMemory: freed <%d> bytes", totalFreed);
#endif
}


//  Given the resource pointer, find the index value and
//  call Reorder.
//
void CResourceManager::Update(CResource *res)
{    
    int32     theIndex;

	theIndex = m_PriorityList.Index(res);
	if (m_PriorityList.ValidIndex(theIndex))
	{
		Reorder(theIndex);
		return;
	}
}

//
//  Reorder - Find the given resource in the list and percolate it up
//  		or down depending upon its newly changed priority status.
//
void CResourceManager::Reorder(int32 inIndex)
{
	PDir		theDir;
	int32		theElement;
	
	if (m_PriorityList.ValidIndex(inIndex))
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
bool CResourceManager::ShouldMove(int32 inIndex, PDir inDir)
{ 
	CResource	*theLower, *theHigher;

	if (m_PriorityList.ValidIndex(inIndex))
	{
		// check endpoint conditions
		if ((inDir == dirUp) and (inIndex == (m_PriorityList.NumItems() - 1)))
			return (FALSE);
		if ((inDir == dirDown) and (inIndex == 0))
			return (FALSE);
		
		if (inDir == dirUp)
		{
			theLower = (CResource *) m_PriorityList.Item(inIndex);
			theHigher = (CResource *) m_PriorityList.Item(inIndex + 1);
		}
		else
		{
			theLower = (CResource *) m_PriorityList.Item(inIndex - 1);
			theHigher = (CResource *) m_PriorityList.Item(inIndex);
		}
		
		return (not (theLower->IsHigher(theHigher)));
	}
	
	return(false);
}

//
//  Swap two items in the array.
//
void CResourceManager::Swap(int32 inIndex1, int32 inIndex2)
{
	CResource		*temp;

	if ((m_PriorityList.ValidIndex(inIndex1)) and (m_PriorityList.ValidIndex(inIndex2)))
	{
    	temp = (CResource *) m_PriorityList.Item(inIndex1);
    	m_PriorityList.Set(m_PriorityList.Item(inIndex2), inIndex1);
    	m_PriorityList.Set(temp, inIndex2);
    }
}
