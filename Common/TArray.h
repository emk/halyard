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

#if !defined (_TArray_h_)
#define _TArray_h_

#include "TCommon.h"
#include "TObject.h"

BEGIN_NAMESPACE_FIVEL

/*-----------------------------------------------------------------

CLASS
    TArray

	An array of TObject objects. 

AUTHOR
    Chuck Officer<br>

-----------------------------------------------------------------*/

class TArray : public TObject 
{
public:
	//////////
	// Constructor.
	//
	// [in_optional] inChunkSize - amount of space in bits to allocate for each
	//			array element (default 8 = 1 byte)
	//
	TArray (int32 inChunkSize = 8);

	//////////
	// Destructor.  Delete the array and all objects within it. 
	//
	virtual 		~TArray();

	//////////
	// Find the index of a TObject in the array.
	//
	// [in] inObject - the TObject we are looking for
	// [out] return - the index of the element, or if the element
	//			is not in the array, return -1.
	//
	int32			Index(TObject *inObject);
	
	//////////
	// Add an element to the array.
	//
	// [in] inObject - element to add to the array
	// [out] return - the index in the array where the elt was placed
	//
	int32			Add(TObject *inObject);

	//////////
	// Overwrite an element in the array.
	//
	// [in] inObject - new element
	// [in] inIndex - index to overwrite
	//
	void			Set(TObject *inObject, int32 inIndex);
	
	//////////
	// Get the number of items in the array.
	//
	// [out] return - number of items in the array
	//
	int32			NumItems();
	
	//////////
	// Is the array empty?
	//
	// [out] return - true if the array is empty, false otherwise
	//
	bool			Empty();
	
	//////////
	// Is the given index valid?
	//
	// [in] inIndex - the index to check
	// [out] return - true if it is a valid index, false otherwise
	//
	bool			ValidIndex(int32 inIndex);
	
	//////////
	// Fetch the element at the given index.
	//
	// [in] inIndex - the index
	// [out] return - the element located at the given index, or NULL if 
	//			inIndex is an invalid index
	//
	TObject			*Item(int32 inIndex);

	//////////
	// Remove an element from the array. This will NOT delete the element.
	// That is assumed to be done elsewhere.
	//
	// [in] inObject - the element to be removed
	//
	void			Remove(TObject *inObject);

	//////////
	// Remove the element with the given inIndex from the array. 
	// The element will NOT be deleted.  That is assumed to be done elsewhere.
	//
	// [in] inIndex - index of the element to be removed
	//
	void			RemoveIndex(int32 inIndex);
	
	//////////
	// Remove all the elements of the array. This will NOT delete the 
	// objects in the array.  That is assumed to be done elsewhere.
	//
	void			RemoveAll();

	//////////
	// Remove an element from the array and delete it.
	//
	// [in] inObject - the element to be deleted
	//
	void			Delete(TObject *inObject);
	
	//////////
	// Remove the element with the given inIndex from the array and delete it. 
	//
	// [in] inIndex - index of the element to be deleted
	//
	void			DeleteIndex(int32 inIndex);
	
	//////////
	// Remove all the elements of the array and delete them. 
	//
	void			DeleteAll();
	
private:
	//////////
	// Array of TObject references. 
	//
	TObjectPtr		*m_List;
	
	//////////
	// Grow the array this much at a time. 
	//
	int32			m_ChunkSize;
	
	//////////
	// Number of items currently in the array. 
	//
	int32			m_NumItems;	
	
	//////////
	// Size of the array. 
	//
	int32			m_ArraySize;
};

inline int32 TArray::NumItems() 
{ 
	return (m_NumItems); 
}
inline bool	TArray::Empty() 
{ 
	return ((m_NumItems > 0) ? false : true); 
}
inline bool	TArray::ValidIndex(int32 inIndex)					
{ 
	return (((inIndex >= 0) and (inIndex < m_NumItems)) ? true : false); 
}
	
inline TObject*	TArray::Item(int32 inIndex)   				
{ 
	return ((ValidIndex(inIndex)) ? m_List[inIndex] : NULL); 
}

END_NAMESPACE_FIVEL

#endif // _TArray_h_

/*
 $Log$
 Revision 1.2  2002/03/04 15:15:54  hamon
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
