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
// TArray.cpp : 
//

#include "THeader.h"
#include "TArray.h"

//
//	TArray - Construct the array.
//
TArray::TArray(int32 inChunkSize)
{
    m_List = NULL;
    m_ChunkSize = inChunkSize;
    m_NumItems = 0;
    m_ArraySize = 0;
}

//
//  ~TArray - Delete the array and all objects within it.
//
TArray::~TArray()
{
	DeleteAll();
    
    if (m_List != NULL)
    	delete [] m_List;
} 

//
//	Index - Return the inIndex of the element. If the element
//		is not in the array, return -1.
//
int32 TArray::Index(TObject* inObject)
{
	int32	i;
	
    for (i = 0; i < m_NumItems; i++)
    {
        if (m_List[i] == inObject) 
        	return (i);
    }

    return (-1);
}

//
//	Add - Add an element to the array and return its inIndex.
//
int32 TArray::Add(TObject* inObject)
{
    TObjectPtr  *tmpArray;
    int32		index;
	int			i;
    
    // make sure it is not already in the array
    if (ValidIndex(index = Index(inObject)))
    	return(index);

    // increase the size of the array, if necessary
    if (m_NumItems >= m_ArraySize) 
    {
    	// allocate the new array
        m_ArraySize += m_ChunkSize;
        tmpArray = new TObjectPtr[m_ArraySize];

	//	if (tmpArray == NULL)
	//		;	// throw memory exception!
        
        // copy over the existing elements
        for (i = 0; i < m_NumItems; i++)
            tmpArray[i] = m_List[i];
        
        // zero out the new ones    
        for (i = m_NumItems; i < m_ArraySize; i++)
        	tmpArray[i] = NULL; 
        
        // toss the old array and remember the new one 
        if (m_List != NULL)   
        	delete [] m_List;
        m_List = tmpArray;
    }

	// add the inObject
	index = m_NumItems;
	m_List[m_NumItems++] = inObject;
	
	return (index);
}

//
//	Set - Overwrite the element at the given inIndex to be the given
//		inObject.
//
void TArray::Set(TObject* inObject, int32 inIndex)
{
	if (ValidIndex(inIndex))
		m_List[inIndex] = inObject;
}

//
//	Remove - Remove an element from the array. This will NOT
//		delete the element. That is assumed to be done 
//		elsewhere.
//
void TArray::Remove(TObject* inObject)
{
    RemoveIndex(Index(inObject));
}

//
//	RemoveIndex - Remove the element with the given inIndex from
//		the array. The element will NOT be deleted.
//
void TArray::RemoveIndex(int32 inIndex)
{
	if (ValidIndex(inIndex))
	{
		// move the remaining elements up one slot
		for (int i = inIndex; i < m_NumItems - 1; i++)
			m_List[i] = m_List[i + 1];
			
		// NULL out the last element
		m_List[--m_NumItems] = NULL;
	}
}		

//
//	RemoveAll - Remove all the elements of the array. This will
//		NOT delete the objects in the array.
//
void TArray::RemoveAll(void)
{	
	for (int i = 0; i < m_NumItems; i++)
		m_List[i] = NULL; 
		
	m_NumItems = 0;
}	

//
//	Delete - Remove the element from the array and then delete
//		it.
//
void TArray::Delete(TObject* inObject)
{ 
	if (inObject != NULL)
	{
    	Remove(inObject);
    	delete inObject;
    }
}

//
//	DeleteIndex - Delete an element given its inIndex.
//		The inIndex is 0 based (like the array).
//
void TArray::DeleteIndex(int32 inIndex)
{
	TObject	*object;
	
	if (ValidIndex(inIndex))
	{
		object = m_List[inIndex];
		RemoveIndex(inIndex);
		delete object;
	}
}

//
//	DeleteAll - Remove all elements from the array and delete
//		them all.
//
void TArray::DeleteAll(void)
{
	TObject	*object;
	
	for (int i = 0; i < m_NumItems; i++)
	{
		object = m_List[i];
		if (object != NULL)
			delete object;
	
		m_List[i] = NULL;
	}
	
	m_NumItems = 0;
}


/*
 $Log$
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
