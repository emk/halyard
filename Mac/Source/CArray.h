/*
	CArray.h

	Class which can manage arrays of a class of objects.
	This is designed for smaller collections where speed
	of access is not critical.
*/

#ifndef _CARRAY_H_
#define _CARRAY_H_

#include "Mac5L.h"
#include "CObject.h"

class CArray : public CObject 
{

	private:
		CObjectPtr		*m_List;
		int32			m_ChunkSize;	//	Grow the array this much at a time.
		int32			m_NumItems;		//	Items in the array.
		int32			m_ArraySize;	//	Size of the array.

	public:
					CArray(int32 inChunkSize = 8);
		virtual 	~CArray();

		int32		NumItems(void) 
						{ return (m_NumItems); }
		bool		Empty(void)
						{ return ((m_NumItems > 0) ? false: true); }
		bool		ValidIndex(int32 inIndex)
						{ return (((inIndex >= 0) and (inIndex < m_NumItems)) ? true: false); }
		int32		Index(CObject *anObject);
		CObject		*Item(int32 inIndex)
						{ return ((ValidIndex(inIndex)) ? m_List[inIndex]: NULL); }

		//	Add objects to the list. Remove objects from the list.
		//	Remove and dispose of objects in the list.
		//	The array's destructor will delete objects in the list.

		int32		Add(CObject *inObject);
		void		Set(CObject	*inObject, int32 inIndex);
		void		Remove(CObject *anObject);
		void		RemoveIndex(int32 inIndex);
		void		RemoveAll(void);
		void		Delete(CObject *inObject);
		void		DeleteIndex(int32 inIndex);
		void		DeleteAll(void);
};

#endif
