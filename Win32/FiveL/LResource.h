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

#if !defined (_LResource_h_)
#define _LResource_h_

#include "TBTree.h"
#include "TArray.h"
#include "TRect.h"
#include "TPoint.h"

//#include "View.h"

//  LResource flags.

//////////
// Resource state<br><br>
// 
// resUnloaded = Data not in memory<br>
// resLocked = Data in memory and may not be removed<br>
// resLoaded = Data in memory; remove if necessary<br>
// resPurgeable = Data in memory; purge as soon as space is needed<br>
//
enum ResState 
{
    resUnloaded,        
    resLocked,          
    resLoaded,          
    resPurgeable        
};

/*-----------------------------------------------------------------

CLASS
    LResource

	A resource that can be loaded and unloaded from memory.  All 
	subclassed should implement _Load() and _Purge(). 

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LResource : public TBNode 
{

    public:
    
        //////////
		// Constructor.  Adds the resource to the priority list.
		//
		LResource(const char *name);

		//////////
		// Destructor.
		//
        virtual 			~LResource() {}

		//////////
		// Return true if the other resource should have a higher
		// purge priority status than this one.<br><br>
		//
		//	Determining purge priority status.<br>
		//
		//	1.  LResource states correspond to initial purge priority.
		//		(Although resUnloaded and resLocked can't be purged.)<br>
		//
		//		resUnloaded < resLocked < resLoaded < resPurgeable<br>
		//
		//	2.  Resources used less than others should be purged first.<br>
		//
		//	3.  Larger resources should be purged first to make more room.<br>
		//
		//	4.  If it's still a tie (same state, same use, same size) than
		//		bugger it and consider the list in order.<br>
		//
		// [in] other - the other resource to compare against
		// [out] return - true(!= 0) if the other resource has a higher priority, 
		//				  false otherwise
		//
        virtual int			IsHigher(LResource *other);
        
		//////////
		// Change the resource's state. If it's unloaded, nothing happens. Otherwise
		// set the state to the new state and update.
		//
		// [in] foo - new resource state
		//
		virtual void		SetState(ResState newState);
        
		//////////
		// Get the state of this resource.
		//
		// [out] return - the ResState of this resource
		//
		virtual ResState	GetState(void) 
				{ return state; }

		//////////
		// Increment the use count for this resource.  Use this method if the resource
		// was used without Load() being called.
		//
		virtual void		Used(void);
        
		//////////
		// Load this resource and increment the use count.
		//
		virtual void		Load(void);
        
		//////////
		// Purge this resource if loaded and update its resState.
		//
		virtual void		Purge(void);
		
		//////////
		// Is this resource loaded?
		//
		// [out] return - true if the resource is loaded, false otherwise
		//
		virtual bool		IsLoaded(void) 
				{ if (state != resUnloaded) return (true); else return (false); }
		
		//////////
		// Is this resource unloaded?
		//
		// [out] return - true if the resource is not loaded into memory, false otherwise
		//
		virtual bool		IsUnloaded(void)
				{ if (state == resUnloaded) return (true); else return (false); }
        
        //////////
		// Is this resource locked?
		//
		// [out] return - true if the resource is locked, false otherwise
		//
        virtual void		Lock(void)
        		{ if (state > resUnloaded) SetState(resLocked); }
        
		//////////
		// Is this resource unlocked?
		//
		// [out] return - true if the resource is unlocked, false otherwise
		//
		virtual void		Unlock(void)
        		{ if (state > resUnloaded) SetState(resLoaded); }
        
		//////////
		// Subclasses must override.  Load the resource into memory.
		//
        virtual void		_Load(void) {}
        
		//////////
		// Subclasses must override.  Purge the resource from memory.
		//
		virtual void		_Purge(void) {}
		
	protected:
        //////////
		// Resource size.
		//
		int32		   		size;

		//////////
		// Use count.
		//
        int32				times_used;
        
		//////////
		// Resource state.
		//
		ResState			state;

};

//////////
// Current priority direction.
//
enum PriorityDirection
{
	dirUp,
	dirDown
};

/*-----------------------------------------------------------------

CLASS
    LResourceManager

	A binary tree of LResource objects.  Manages a set of resources.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LResourceManager : public TBTree
{
public:
	//////////
	// Constructor.
	//
	LResourceManager();
	
	//////////
	// Destructor.
	//
	~LResourceManager();

	//////////
	// Try to free memory for at least inMemory bytes of storage.
	//
	// [in] inMemory - amount of memory needed in bytes
	//
	void			RequestMemory(int32 inMemory);
	
	//////////
	// Remove all resources.
	//
	void			RemoveAll();

	//////////
	// Get an managed LResource by name.
	//
	// [in] inName - name of the resource
	// [out] return - an LResource object ptr
	//
	LResource		*GetResource(TString &inName);
	
	//////////
	// Update the priority list.  Move the given resource up or down as necessary.
	//
	// [in] inRes - a resource to move up or down in the priority list
	//
	virtual void	UpdatePriority(LResource *inRes);
	
	//////////
	// Add a resource to the tree.
	//
	// [in] inRes - an LResource to add to the tree
	//
	virtual void	AddNode(LResource *inRes);	

protected:
	//////////
	// Resource priority list.
	//
	TArray			m_PriorityList;

	//////////
	// Find the given resource in the list and percolate it up
	// or down depending upon its newly changed priority status.
	//
	// [in] inIndex - index of the resource we are interested in
	//
	virtual void	Reorder(int32 inIndex);
	
	//////////
	// Should the specified resource move in the given direction?
	//
	// [in] inIndex - index of the resource
	// [in] inDir - direction
	// [out] return - true if the given item should move in the given
	//				  direction, false otherwise.
	//
	virtual bool	ShouldMove(int32 inIndex, PriorityDirection inDir);
	
	//////////
	// Swap two items in m_PriorityList.
	//
	// [in] inIndex1 - item #1
	// [in] inIndex2 - item #2
	//
	virtual void	Swap(int32 inIndex1, int32 inIndex2);
	
	//////////
	// Make sure we have enough memory to read in inMem bytes.
	//
	// [in] inMem - number of bytes we need to be able to read into memory
	//
	virtual void	FreeMemory(int32 inMem);
};

#endif // _LResource_h_

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  2000/02/02 15:15:32  chuck
 no message

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
