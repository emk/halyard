//
//	CResource.cp - Implementation of CResource class.
//

#include "CResource.h"
#include "CList.h"

CBTree   			gResManager;    //  B-Tree containing all resources.
// cbo_mem
CList    			PriorityList;   //  Resource priority list.

/*********************

    RESOURCE CLASS

*********************/

//  Add the resource to the priority list. The name should include the
//  suffix (.GFT, .PCX).
//  
CResource::CResource(const char *name) : CBNode(name)
{
    size = 0;
    times_used = 0;
    state = kResUnloaded;

	// cbo_mem
    PriorityList.Add(this);
    UpdatePriority();
}

//
//	Lock - If inLock is true and the resource is in memory, lock it
//		there. If inLock is false and the resource is in memory, make
//		sure it isn't locked.
//
void CResource::Lock(bool inLock)
{
	if (state > kResUnloaded)		// make sure it is in memory first
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
    if (state > kResUnloaded && state != newState) 
    {
        state = newState;
       	UpdatePriority();
    }
}

//  We was used! Bump the counter and move the object if necessary.
//  Most of the time calling Load() is enough since that bumps the
//  counter as well, but if there is some aspect of your subclass
//  that counts as a "use" without calling Load(), you can call Used()
//  to bump the counter.
//
void CResource::Used()
{
    times_used++;
    UpdatePriority();
}

//  Confirm enough memory is available to load the resource,
//  then load it. Loading a resource counts as a use, so bump
//  use counter as well.
//
void CResource::Load()
{
    if (state == kResUnloaded) 
    {
#ifdef DEBUG_5L
	//	prinfo("loading resource <%s>, size <%ld>", key.GetString(), size);
#endif

		// cbo_mem - don't do this if not trying to manage memory in resource
		// tree
    	PriorityList.FreeMemory(size);
    	
        _Load();
        state = kResLoaded;
    }

	times_used++;
	UpdatePriority();
}

//  Purge if loaded; update priority.
//
void CResource::Purge()
{
    if (state > kResLocked) 
    {
        _Purge();
        state = kResUnloaded;
        UpdatePriority();
    }
}

//  This resource's priority has changed because its load status,
//  purge status, lock status, or use value has changed.
//
void CResource::UpdatePriority()
{
	// cbo_mem
     PriorityList.Update(this); 
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
    if (state != other->state)
        return (other->state > state);

    if (times_used != other->times_used)
        return (other->times_used < times_used);

    if (size != other->size)
        return (other->size > size);
    
    return (TRUE);
}

//
// utility functions
//
//  Get the given resource, or return 0 if not found.
//  Every time a resource is requested, consider it used
//  and bump its count.
//
CResource *GetResource(const char *name)
{
    CResource    *res;

    //  FindNode may return 0.
    //
    res = (CResource *) gResManager.FindNode(name, TRUE);
    return res;
}

void KillResTree(void)
{
	gResManager.ZapTree();
	// cbo_mem
	PriorityList.RemoveAll();
}

int32 FreeUpSpace(int32 bytesNeeded)
{
	return (PriorityList.FreeUpMemory(bytesNeeded));
}




