/****************************************

    Binary Tree source.

****************************************/
#include "debug.h"

#include "CBTree.h"
#include "util.h"

/**************************

    BINARY NODE METHODS

**************************/

//  Create the instance variables. Don't worry about placement
//  yet; CBTree will do that.
//
CBNode::CBNode(const char *newKey)
{
    if (newKey)
        key = newKey;
    left = right = NULL;
}

//  Delete the node data as well as the key as well as
//  the leaves.
//
CBNode::~CBNode()
{

}

/***********************************************************************
 * Function: CBNode::SetKey
 *
 *  Parameter newKey  (new "this" key)
 * Return:
 *      
 * Comments: Set the key for the node.
 *
 ***********************************************************************/
void CBNode::SetKey(const char *newKey)
{
    key = newKey;
}

/***********************************************************************
 * Function: CBNode::Add
 *
 *  Parameter node    (to add)
 * Return:
 *
 * Comments: Add a new node onto this one. Figure out where it goes and
 *  then add it.
 ***********************************************************************/
void CBNode::Add(CBNode *node)
{
    int     res = key.Compare(node->key, FALSE);

	if (res < 0)
		if (right == NULL) 
			right = node;
		else 
			right->Add(node);
	 else if (res > 0)
		if (left == NULL) 
			left = node;
		else 
			left->Add(node);
	else
        prerror("There is already a node with key %s", (char *)key);
}

/***********************************************************************
 * Function: CBNode::FindMin
 *
 *  Parameter (null)
 * Return:
 *      Node that's the next smallest compared to "this"
 * Comments:
 *      Ret. val could be NULL.  Used by Remove Node
 ***********************************************************************/
CBNode * CBNode::FindMin()
{
    CBNode * cursor=this, *TmpNode;

    if (not cursor->left) 
    	return cursor;
    for (;cursor->left->left;cursor = cursor->left) 
    	;
    TmpNode = cursor->left;
    cursor->left = cursor->left->right;
    return TmpNode;
}

/***********************************************************************
 * Function: CBNode::Remove
 *
 *  Parameter (null)
 * Return:
 *     Returns the subtree formed when "this" is removed.
 * Comments:
 *      Takes care of boundary cond's
 ***********************************************************************/
CBNode * CBNode::Remove()
{
    CBNode *TmpNode, *Tmp;

    if (not this) 
    	return this; //Null node to delete...
    
    if (not right) 
    {
        TmpNode = left;
        delete this;

        return (TmpNode);
    }
    Tmp = right->left;
    TmpNode = right->FindMin();
   
    if (Tmp)    //FindMin is NOT the same with this->right
        TmpNode->right=right;
    TmpNode->left=left;
    delete this;

    return (TmpNode);
}

/***********************************************************************
 * Function: CBNode::FindforRemove
 *
 *  Parameter Nodename  (to remove)
 * Return:
 *    Subtree resulting from removal of "Nodename" from "this" tree
 * Comments:
 *    Classical BTree removal procedure..
 ***********************************************************************/
CBNode * CBNode::FindforRemove(CString &Nodename)
{
    CBNode 	*TmpNode = this;
    int 	res;

    if (not this) 
    {
        prerror("Delete from Empty Tree!");
        return (NULL);
    }
    else  
    {
        res = TmpNode->key.Compare(Nodename,FALSE);
        
        if (res == 0) 
        	return (Remove());
        
        for ( ; TmpNode; res = TmpNode->key.Compare(Nodename,FALSE))   
        {
            if (res < 0)  
            {
                if (not TmpNode->right) 
                	return (NULL);
                else  
                {
                    if (not TmpNode->right->key.Compare(Nodename,FALSE)) 
                    {
                        TmpNode->right = TmpNode->right->Remove();
                        return (this);
                    }
                    else 
                    	TmpNode=TmpNode->right;
                }
            }
            if (res > 0)  
            {
                if (not TmpNode->left) 
                	return (NULL);
                else  
                {
                    if (not TmpNode->left->key.Compare(Nodename,FALSE)) 
                    {
                        TmpNode->left = TmpNode->left->Remove();
                        return (this);
                    }
                    else 
                    	TmpNode = TmpNode->left;
                }
            }
        }
		
		return (this);
    }
}   


/***********************************************************************
 * Function: CBNode::Find
 *
 *  Parameter searchKey
 * Return:
 *  Pointer to node with key "searchKey"
 * Comments:
 *  Search for a node by its key. Case is irrelevent.
 ***********************************************************************/
CBNode * CBNode::Find(const char *searchKey)
{
    int16 res = key.Compare(searchKey, FALSE);

    if (res < 0)
    {
        if (right == NULL) 
        	return (NULL);
        else 
        	return (right->Find(searchKey));
    }
    else if (res > 0)
    {
        if (left == NULL) 
        	return (NULL);
        else 
        	return (left->Find(searchKey));
    }
    else
        return (this);
}

/***********************************************************************
 * Function: CBNode::Report
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *    when used, it gives node debug info.
 ***********************************************************************/
void CBNode::Report()
{
    //added 11MAR94
    // DebugClose();
    // DebugOpen();
    // DebugFile("\nThis node's address is %ld\n", (long)this);
    // DebugFile("Address stored in right ponter is %ld\n", (long)(this->right));
    // DebugFile("Address stored in left ponter is %ld\n", (long)(this->left));
    // DebugFile("Node: %s\n", (char *)key);
}

/***********************************************************************
 * Function: CBNode::Walk
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *   inorder traversal of current subtree "this"
 ***********************************************************************/
void CBNode::Walk()
{
    if (left) 
    	left->Walk();
    	
    Report();
    
    if (right) 
    	right->Walk();
}

/***********************************************************************
 * Function: CBNode::RemoveAll
 *
 *  Parameter rootnode
 * Return:
 *
 * Comments:
 *      MAR 30, 1994  Destroys the subtree located at "this"
 ***********************************************************************/
void CBNode::RemoveAll(CBNode * rootnode)
{
    if (not this) 
    	return;
    
    if (left)  
    	left->RemoveAll(rootnode);
    
    if (right) 
    	right->RemoveAll(rootnode);
    
    if (this != rootnode) 
    	delete this;
}
/**************************

    BINARY TREE METHODS

**************************/

CBTree::CBTree()
{
    root = NULL;
}

CBTree::~CBTree()
{
    delete root;
}

/***********************************************************************
 * Function: CBTree::ZapTree
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *      //MAR 30.  Added to dispose of all managers. Deletes current tree.
 ***********************************************************************/
void CBTree::ZapTree()
{
	if (root != NULL)
	{
		root->RemoveAll(root);
		delete root;
		root = NULL;
	}
}

/***********************************************************************
 * Function: CBTree::AddNode
 *
 *  Parameter newNode (to add)
 * Return:
 *
 * Comments:
 *      Add a node to the tree. Might be the root, so check.
 ***********************************************************************/
void CBTree::AddNode(CBNode *newNode)
{
    if (root == NULL)
        root = newNode;
    else
        root->Add(newNode);
}

/***********************************************************************
 * Function: CBTree::RemoveNode
 *
 *  Parameter NodeName
 * Return:
 *
 * Comments:
 *     Removes node by name.
 ***********************************************************************/
void CBTree::RemoveNode(CString &NodeName)
{
    if (root == NULL)
        prerror("The CBTree is empty,  Couldn't remove the node");
    else   
    {
        if (root->Find(NodeName.GetString()))  
            root = root->FindforRemove(NodeName);
        else 
        	prerror("No Such Node!");
    }
}

/***********************************************************************
 * Function: CBTree::FindNode
 *
 *  Parameter searchKey
 *  Parameter oktofail (if true, just return NULL on fail find, else barf)
 * Return:
 *      Pointer to node with "searchKey" or NULL
 * Comments: Find a node by its key. Return its data: the object.
 *  This can return NULL if it cannot find the node!!!
 *
 ***********************************************************************/
CBNode *CBTree::FindNode(const char *searchKey, int oktofail)
{
    CBNode   *node;

    if (root == NULL) 
    	node = NULL;
    else 
    	node = root->Find(searchKey);

    if (node || oktofail) 
    	return (node);
    
    //prerror("Could not find node <%s>", searchKey);
    return (NULL);
}

/***********************************************************************
 * Function: CBTree::Walk
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *  New function added 08MAR94 to get output of CBTree
 ***********************************************************************/
void CBTree::Walk()
{
    root->Walk();
}   



 
