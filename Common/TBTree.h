// -*- Mode: C++; tab-width: 4; -*-
//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999,2000 Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// TBTree.h : 
//

#if !defined (_TBTree_h_)
#define _TBTree_h_

#include "TCommon.h"
#include "TObject.h"
#include "TString.h"

BEGIN_NAMESPACE_FIVEL

/*-----------------------------------------------------------------

CLASS
    TBNode

	A node in a TBTree. 

AUTHOR
    Chuck Officer<br>

-----------------------------------------------------------------*/
class TBNode : public TObject 
{
public:
	//////////
	// Constructor.
	//
	TBNode();

	//////////
	// Constructor.
	//
	// [in] inKey - a key (name) to describe this node
	//
	TBNode(const char *inKey);
	
	//////////
	// Constructor.
	//
	// [in] inKey - a key (name) to describe this node
	//
	TBNode(TString &inKey);
	
	//////////
	// Destructor.
	//
	~TBNode();    

	//////////
	// Get the key (name) for this node.
	//
	// [out] return - the key for this node
	//
	inline const char *Key() { return ((const char *) m_Key); }

    //////////
	// Add a node to this one. Figures out where it goes and makes the links.
	//
	// [in] inNode - new node to attach as a child of this node
	// [out] return - true if successful, false otherwise
	//
	bool			Add(TBNode *inNode);

    //////////
	// Find the next smallest node compared to this one.
	//
	// [out] return - the next smallest node
	//
	TBNode			*FindMin(void);
    
	//////////
	// Remove (and delete) this node and return the subtree that is formed.
	//
	// [out] return - the subtree formed
	//
	TBNode			*Remove(void);
	
	//////////
	// Find a node in this subtree and remove it.
	//
	// [in] inKey - key for the node to be removed
	// [out] return - the node that was removed, or NULL if not found
	//
	TBNode			*FindAndRemove(const char *inKey);

    //////////
	// Find a node in this subtree.
	//
	// [out] return - the node that was found, or NULL if not found
	//
	TBNode			*Find(const char *inKey);

    //////////
	// Destroy this subtree.
	//
	// [in] inRoot - the root of the tree, which will not be deleted
	//
	void			RemoveAll(TBNode *inRoot);

protected:	
	//////////
	// A key which identifies this node.
	//
	TString			m_Key;

	//////////
	// Initialize this node and its left and right child pointers.
	//
	void			InitMembers(void);

	//////////
	// Left child.
	//
	TBNode			*m_Left;
	
	//////////
	// Right child.
	//
	TBNode			*m_Right;

};


/*-----------------------------------------------------------------

CLASS
    TBTree

	A binary tree consisting of TBNode objects. 

AUTHOR
    Chuck Officer<br>

-----------------------------------------------------------------*/
class TBTree : public TObject 
{
public:    
	//////////
	// Constructor.
	//
	TBTree();
	
	//////////
	// Destructor.
	//
	~TBTree();
  
    //////////
	// Add a new node to the tree.
	//
	// [in] inNode - new node
	//
	void			Add(TBNode *inNode);

	//////////
	// Get the root of this tree.
	//
	// [in] return - the root TBNode of this tree, or NULL if not found
	//
	TBNode			*GetRoot();

    //////////
	// Find a node in the tree.
	//
	// [in] inKey - key (name) of the node
	// [out] return - the node found, or NULL if not found
	//
	TBNode			*Find(const char *inKey);

	//////////
	// Remove a node from the tree.
	//
	// [in] inKey - the key (name) of the node to be removed
	//
	void			Remove(const char *inKey);
	
	//////////
	// Remove all node from the tree.
	//
	void			RemoveAll(void);

protected:
	//////////
	// Root node of the tree.
	//
	TBNode			*m_Root;
};

END_NAMESPACE_FIVEL

#endif // _TBTree_h_

/*
 $Log$
 Revision 1.2.4.1  2002/04/29 06:19:11  emk
 Some over-the-weekend performance tuning.

 - Added fonttools/fontspeed.cpp, which mimics 5L drawing patterns, but with
 an empty DrawPixMap routine.

 - Added a pre-rendered glyph cache to the Typography module.

 - Added new features to GraphicsTools to support the glyph cache.

 - visual-test.png has apparently changed, but I can't see any difference.
 It's probably slight changes in anti-aliased pixel intensity.

 - Miscellaneous other cleanups and tweaks.

 Revision 1.2  2002/03/04 15:15:56  hamon
 Added support for compiler's namespaces. Namespaces are only enabled on macintosh.

Moved OS specific configuration to TPlatform.h

Changes by Elizabeth and Eric, okayed by Eric.

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.1  2000/04/06 17:06:10  chuck
 Initial check-in

*/
