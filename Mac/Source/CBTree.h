/*************************************

    Binary tree and node classes.

    This particular tree uses strings
    to determine node order.

*************************************/

#ifndef _H_CBTREE

#define _H_CBTREE

#include "CObject.h"
#include "CString.h"

class CBNode : public CObject 
{
    protected:
        //CString key;
        CBNode       *left, *right;

    public:
        CString key;
        
        CBNode(const char *newkey = NULL);
        virtual ~CBNode();    

        virtual void SetKey(const char *newKey);
        virtual void Add(CBNode *node);
        //added 19MAR94
        CBNode * FindMin();
        CBNode * Remove();
        CBNode * FindforRemove(CString &Nodename);
        CBNode *Find(const char *searchKey);
        virtual void Walk(void);
        virtual void Report(void);
        virtual void RemoveAll(CBNode *root);        //Added MAR 30
};

class CBTree : public CObject 
{

//  protected:

//      CBNode   *root;

    public:
        CBNode 	*root;
        
        				CBTree();
        virtual 		~CBTree();
        virtual void	ZapTree();
        virtual void 	AddNode(CBNode *newNode);
        virtual CBNode *FindNode(const char *searchKey, int oktofail = FALSE);
        virtual void 	Walk(); // New function added to access Bnode walk functio
        //added 18MAR94
        void RemoveNode(CString &NodeName);
};

#endif
