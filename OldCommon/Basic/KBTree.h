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
// KBTree.h : 
//

#if !defined (_KBTree_h_)
#define _KBTree_h_

#include "KCommon.h"
#include "KObject.h"
#include "KString.h"

class KBNode : public KObject 
{
public:
					KBNode();
					KBNode(const char *inKey);
					KBNode(KString &inKey);
					~KBNode();    

	inline const char *Key() { return ((const char *) m_Key); }

    bool			Add(KBNode *inNode);

    KBNode			*FindMin(void);
    KBNode			*Remove(void);
	KBNode			*FindAndRemove(const char *inKey);

    KBNode			*Find(const char *inKey);

    void			RemoveAll(KBNode *inRoot);

protected:
	void			InitMembers(void);

	KBNode			*m_Left;
	KBNode			*m_Right;
	KString			m_Key;

};

class KBTree : public KObject 
{
public:    
					KBTree();
					~KBTree();
  
    void			Add(KBNode *inNode);

	KBNode			*GetRoot();

    KBNode			*Find(const char *inKey);

	void			Remove(const char *inKey);
	void			RemoveAll(void);

protected:
	KBNode			*m_Root;
};

#endif // _KBTree_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
