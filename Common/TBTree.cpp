// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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
// TBTree.cpp : 
//

#include "CommonHeaders.h"

#include "THeader.h"
#include "TBTree.h"

USING_NAMESPACE_FIVEL

//
//	TBNode
//
TBNode::TBNode()
{
	InitMembers();
}

TBNode::TBNode(const char *inKey)
{
	InitMembers();
	m_Key = inKey;
}

TBNode::TBNode(TString &inKey)
{
	InitMembers();
	m_Key = inKey;
}

void TBNode::InitMembers(void)
{
	m_Key.Empty();
	m_Left = NULL;
	m_Right = NULL;
}

TBNode::~TBNode()
{
	
}

//
//	Add - Add a node to this one. Figure out where it goes and
//		put it there.
//
bool TBNode::Add(TBNode *inNode)
{
    int32 result = m_Key.Compare(inNode->Key(), false);

	// Make sure our new node has no children attached.
	ASSERT(inNode->m_Left == NULL);
	ASSERT(inNode->m_Right == NULL);
	
    if (result < 0)
    {
        if (m_Right == NULL) 
        	m_Right = inNode;
        else 
        	m_Right->Add(inNode); 
	}
    else if (result > 0)
    {
        if (m_Left == NULL) 
        	m_Left = inNode;
        else 
        	m_Left->Add(inNode);
	}
    else
	{
		// XXX - We'd like to do a throw() here, but it seems to
		// cause a bunch of yucky errors.  So just die for now.
		gLog.FatalError("Duplicate node <%s>.", inNode->Key());
	}

	return true;
}

//
//	Remove - Remove this node and return the sub-tree that is 
//		formed.
//
TBNode *TBNode::Remove(void)
{
	ASSERT(this != NULL);

	// Figure our how to update our tree.
    TBNode	*retNode;
	if (m_Right == NULL) 
    {
		// CASE 1: We have no right child, so promote the left.
        retNode = m_Left;
    }
	else if (m_Left == NULL)
	{
		// CASE 2: We have no left child, so promote the right.
		retNode = m_Right;
	}
	else if (m_Right->m_Left == NULL)
	{
		// CASE 3: Our right child has no left child, so
		// promote it and attach *our* left child.
		retNode = m_Right;
		m_Right->m_Left = m_Left;
	}
	else
	{
		// CASE 4: We're going to have to do some hard work.
		// Find the smallest node in our right-hand tree,
		// detach it, and use it to replace us.
		TBNode	*cursor = m_Right;
		ASSERT(cursor->m_Left != NULL);

		// Iterate down the left-hand branch of our tree until
		// cusor->m_Left points to a node with no left child.
		// At this point, cursor->m_Left is the smallest node
		// in our right-hand subtree (the one we're looking for!).
		while (cursor->m_Left->m_Left != NULL)
			cursor = cursor->m_Left;
		retNode = cursor->m_Left;

		// Patch up the hole left by removing cursor->m_Left.
		cursor->m_Left = cursor->m_Left->m_Right;

		// Finish hooking up our replacement node.
		retNode->m_Left = m_Left;
		retNode->m_Right = m_Right;
	}

	// Delete the current node and return our result.
    delete this;
    return retNode;
}

//
//	FindAndRemove - 
//
TBNode *TBNode::FindAndRemove(const char *inKey, bool *outWasFound)
{
	ASSERT(this != NULL);

	// Compare our search string against the current node's key, and decide
	// what to do.
    int32 comparison_result = m_Key.Compare(inKey, false);
	if (comparison_result == 0)
	{
		// CASE 1: The current node matches our search key.
		*outWasFound = true;
		return Remove();
	}
	else if (comparison_result < 0)
	{
		// CASE 2: The current node is less than our search key.
		if (m_Right == NULL)
			*outWasFound = false;
		else
			m_Right = m_Right->FindAndRemove(inKey, outWasFound);
	}
	else // if (comparison_result > 0)
	{
		// CASE 3: The current node is greater than our search key.
		if (m_Left == NULL)
			*outWasFound = false;
		else
			m_Left = m_Left->FindAndRemove(inKey, outWasFound);
	}

	// Since this node wasn't deleted, we don't need to replace
	// it in the tree.  Therefore, just return ourself.
	return this;
}

//
//	Find
//
TBNode *TBNode::Find(const char *inKey)
{
	if (inKey == NULL)
		return (NULL);

    int32 result = m_Key.Compare(inKey, false);

    if (result < 0)
    {
        if (m_Right == NULL) 
        	return (NULL);
        else 
        	return (m_Right->Find(inKey));
    }
    else if (result > 0)
    {
        if (m_Left == NULL) 
        	return (NULL);
        else 
        	return (m_Left->Find(inKey));
    }
    else
        return (this);
}

//
//	RemoveAll - Destroy this sub-tree.
//
TBNode *TBNode::RemoveAll()
{
    ASSERT(this != NULL);

    if (m_Left != NULL)
		m_Left->RemoveAll();

    if (m_Right != NULL)
		m_Right->RemoveAll();

	delete this;
	return NULL;
}

//
//	TBTree methods
//

TBTree::TBTree()
{
    m_Root = NULL;
}

TBTree::~TBTree()
{
	RemoveAll();
}

//
//	Root - Return the root node.
//
TBNode *TBTree::GetRoot()
{
	return (m_Root);
}

//
//	Add - Add a node to the tree.
//
void TBTree::Add(TBNode *inNode)
{
	ASSERT(inNode->m_Left == NULL);
	ASSERT(inNode->m_Right == NULL);

    if (m_Root == NULL)
        m_Root = inNode;
    else
        m_Root->Add(inNode);
}

//
//	Find - Find a node.
//
TBNode *TBTree::Find(const char *inKey)
{
    if (m_Root != NULL)
    	return m_Root->Find(inKey);
	else
		return NULL;
}

//
//	Remove - Remove the node with inKey from
//		the tree.
//
void TBTree::RemoveIfExists(const char *inKey)
{
	if (m_Root != NULL)
	{
		bool found;
		m_Root = m_Root->FindAndRemove(inKey, &found);
	}
}

//
// RemoveAll - Remove everything in this tree.
//
void TBTree::RemoveAll(void)
{
    if (m_Root != NULL)
    	m_Root = m_Root->RemoveAll();
}

/*
 $Log$
 Revision 1.8  2003/06/13 10:57:30  emk
 Further use of precompiled headers; pruning of various inappropriate
 includes.

 Revision 1.7  2002/07/26 19:18:34  emk
 Make an error fatal, and tweak the release notes.

 Revision 1.6  2002/07/25 22:25:25  emk
   * Made new CryptStream auto_ptr code work under Windows.
   * PURIFY: Fixed memory leak in TBTree::Add of duplicate node.  We now
     notify the user if there are duplicate cards, macros, etc.
   * PURIFY: Fixed memory leak in TBTree destructor.
   * PURIFY: Fixed memory leak in ConfigManager destructor.
   * PURIFY: Fixed memory leaks when deleting DIBs.
   * PURIFY: Made sure we deleted offscreen GWorld when exiting.
   * PURIFY: Fixed memory leak in LBrowser.
   * PURIFY: Fixed memory leak in LFileBundle.
   * PURIFY: Fixed uninitialized memory reads when View methods were
     called before View::Init.
   * PURIFY: Made View::Draw a no-op before View::Init is called.
     (It seems that Windows causes us to call Draw too early.)

 Revision 1.5  2002/05/29 09:38:53  emk
 Fixes for various "crash on exit" bugs in 5L.

   * Fixed lots of bugs in TBTree, mostly in the code for removing nodes.
     TBTree should now work more or less correctly.
   * Removed the broken reference counting logic in TIndex and TIndexFile.
   * Made FatalError call abort(), not exit(1), so the destructors for
     (possibly corrupt) global variables will not be called.

 This code may break either the Windows or Mac build; I'll try to fix things
 right away.

 Revision 1.4  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.3.4.1  2002/04/29 06:19:11  emk
 Some over-the-weekend performance tuning.

 - Added fonttools/fontspeed.cpp, which mimics 5L drawing patterns, but with
 an empty DrawPixMap routine.

 - Added a pre-rendered glyph cache to the Typography module.

 - Added new features to GraphicsTools to support the glyph cache.

 - visual-test.png has apparently changed, but I can't see any difference.
 It's probably slight changes in anti-aliased pixel intensity.

 - Miscellaneous other cleanups and tweaks.

 Revision 1.3  2002/03/04 15:15:55  hamon
 Added support for compiler's namespaces. Namespaces are only enabled on macintosh.Moved OS specific configuration to TPlatform.hChanges by Elizabeth and Eric, okayed by Eric.

 Revision 1.2  2002/02/27 16:38:21  emk
 Cross-platform code merge!

 * Merged back in support for the Macintosh platform.  This is an ongoing
   process, and we'll need to do more work.

 * Separated out platform specific configuration with big block comments.

 * Merged in a few changes from KBTree which appeared to fix bugs.

 * Merged in IntToString, UIntToString, DoubleToString changes from the
   Macintosh engine, and improved the error handling.  NOTE: doubles now
   print using "%f" (the way the Mac engine always did it).  This means
   that "tstr = 0.0" will set 'tstr' to "0.000000", not "0" (as it
   did in the Win32 engine).

 This code might not build on Windows.  As soon as I hear from ssharp
 that he's updated the project files, I'll test it myself.

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
