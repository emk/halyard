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
		return (false);

	return (true);
}

//
//	FindMin - Find the next smallest node compared to 
//		this one.
//
TBNode *TBNode::FindMin(void)
{
	TBNode	*cursor = this;
	TBNode	*retNode;

	if (cursor->m_Left == NULL) 
    	return (cursor);

	while (cursor->m_Left->m_Left != NULL)
		cursor = cursor->m_Left;

    retNode = cursor->m_Left;

    cursor->m_Left = cursor->m_Left->m_Right;

    return (retNode);
}

//
//	Remove - Remove this node and return the sub-tree that is 
//		formed.
//
TBNode *TBNode::Remove(void)
{
    TBNode	*retNode;
	TBNode	*tmpNode;

    if (this == NULL) 
    	return this;

    if (m_Right == NULL) 
    {
        retNode = m_Left;
        delete this;
        return (retNode);
    }

    tmpNode = m_Right->m_Left;
    retNode = m_Right->FindMin();

    if (tmpNode != NULL)
        retNode->m_Right = m_Right;
    retNode->m_Left = m_Left;

    delete this;
    
    return (retNode);
}

//
//	FindAndRemove - 
//
TBNode *TBNode::FindAndRemove(const char *inKey)
{
	TBNode	*tmpNode = this;
    int32 	result;

    result = m_Key.Compare(inKey, false);

    if (result == 0) 
		return (Remove());		// this is the one we are going to remove

    for ( ; tmpNode != NULL; result = tmpNode->m_Key.Compare(inKey, false))   
    {
        if (result < 0)  
        {
            if (tmpNode->m_Right != NULL) 
                return (NULL);
            else  
            {
                if (tmpNode->m_Right->m_Key.Compare(inKey, false) == 0) 
                {
                    tmpNode->m_Right = tmpNode->m_Right->Remove();
                    return (this);
				}
                else 
                    tmpNode = tmpNode->m_Right;
            }
        }
        else if (result > 0)  
        {
            if (tmpNode->m_Left != NULL) 
                return (NULL);
            else  
            {
                if (tmpNode->m_Left->m_Key.Compare(inKey, false) == 0) 
                {
                    tmpNode->m_Left = tmpNode->m_Left->Remove();
                    return (this);
                }
                else 
                    tmpNode = tmpNode->m_Left;
            }
        }
    }

    return (this);
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
void TBNode::RemoveAll(TBNode *inRoot)
{
    if (this == NULL) 
    	return;

    if (m_Left != NULL)
	{
		m_Left->RemoveAll(inRoot);
		m_Left = NULL;
	}

    if (m_Right != NULL)
	{
		m_Right->RemoveAll(inRoot);
		m_Right = NULL;
	}

    if (this != inRoot) 
    	delete this;
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
    if (m_Root != NULL) 
    	delete m_Root;
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
    TBNode   *retNode = NULL;

    if (m_Root != NULL)
    	retNode = m_Root->Find(inKey);

    return (retNode);
}

//
//	Remove - Remove the node with inKey from
//		the tree.
//
void TBTree::Remove(const char *inKey)
{
	if (m_Root != NULL)
	{
		if (m_Root->Find(inKey))
			m_Root = m_Root->FindAndRemove(inKey);
	}
}

//
// RemoveAll - Remove everything in this tree.
//
void TBTree::RemoveAll(void)
{
    if (m_Root != NULL)
    { 
    	m_Root->RemoveAll(m_Root);
    	delete m_Root;
    	m_Root = NULL;
    }
}

/*
 $Log$
 Revision 1.3  2002/03/04 15:15:55  hamon
 Added support for compiler's namespaces. Namespaces are only enabled on macintosh.

Moved OS specific configuration to TPlatform.h

Changes by Elizabeth and Eric, okayed by Eric.

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
