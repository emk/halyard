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
// KArray.h : 
//

#if !defined (_KArray_h_)
#define _KArray_h_

#include "KCommon.h"
#include "KObject.h"

class KArray : public KObject 
{
public:
					KArray (int32 inChunkSize = 8);
	virtual 		~KArray();

	int32			Index(KObject *inObject);
	int32			Add(KObject *inObject);
	void			Set(KObject *inObject, int32 inIndex);
	int32			NumItems();
	bool			Empty();
	bool			ValidIndex(int32 inIndex);
	KObject			*Item(int32 inIndex);

	void			Remove(KObject *inObject);
	void			RemoveIndex(int32 inIndex);
	void			RemoveAll();

	void			Delete(KObject *inObject);
	void			DeleteIndex(int32 inIndex);
	void			DeleteAll();
	
protected:
	KObjectPtr		*m_List;		//	Array of inObject references.
	int32			m_ChunkSize;	//	Grow the array this much at a time.
	int32			m_NumItems;		//	Items in the array.
	int32			m_ArraySize;	//	Size of the array.
};

inline int32 KArray::NumItems() 
{ 
	return (m_NumItems); 
}
inline bool	KArray::Empty() 
{ 
	return ((m_NumItems > 0) ? false : true); 
}
inline bool	KArray::ValidIndex(int32 inIndex)					
{ 
	return (((inIndex >= 0) and (inIndex < m_NumItems)) ? true : false); 
}
	
inline KObject*	KArray::Item(int32 inIndex)   				
{ 
	return ((ValidIndex(inIndex)) ? m_List[inIndex] : NULL); 
}

#endif // _KArray_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
