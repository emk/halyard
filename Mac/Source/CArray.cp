/*
    CArray.cpp

*/

#include "CArray.h"

CArray::CArray(int32 inChunkSize)
{
    m_List = NULL;
    m_ChunkSize = inChunkSize;
    m_NumItems = 0;
    m_ArraySize = 0;
}

//  Delete the array and all objects within it.
//
CArray::~CArray()
{
	CObject		*theObj;
    int32		i;    

    for (i = 0; i < m_NumItems; i++) 
    {
        theObj = m_List[i];
        if (theObj != NULL)
        	delete theObj;
    }
    
    if (m_List != NULL)
    	delete m_List;
}

//
//	Index - Return the index of the object.
//
int32 CArray::Index(CObject *inObject)
{
	int32		i;
	
	for (i = 0; i < m_NumItems; i++)
	{
		if (m_List[i] == inObject)
			return (i);
	}
	
	return (-1);
}

//
//	Add - Add an element to the array.
//
int32 CArray::Add(CObject *inObject)
{
    CObjectPtr  *theTmpArray;
    int32		retIndex = -1;
    int32		i;

	// make sure the item isn't already in the list
    if (ValidIndex(Index(inObject)))
        return (retIndex);

    //  Increase array size if necessary.
    if (m_NumItems >= m_ArraySize) 
    {
        m_ArraySize += m_ChunkSize;
        theTmpArray = new CObjectPtr[m_ArraySize];
        
        for (i = 0; i < m_NumItems; i++)
            theTmpArray[i] = m_List[i];
            
        for (i = m_NumItems; i < m_ArraySize; i++)
        	theTmpArray[i] = NULL;
        
		if (m_List != NULL)	
			delete m_List;
        m_List = theTmpArray;
    }

    //  add the object to the end of the list  
    retIndex = m_NumItems;
    m_List[m_NumItems] = inObject;
    m_NumItems++;
    
    return (retIndex);
}

//
//	Set - Overwrite the element at the given index to be the given
//			object. The original object is NOT destroyed.
//
void CArray::Set(CObject *inObject, int32 inIndex)
{
	if (ValidIndex(inIndex))
		m_List[inIndex] = inObject;
}

//
//	Remove - Remove an element from the array.
//
void CArray::Remove(CObject *inObject)
{
	int32	theIndex;

    theIndex = Index(inObject);
    RemoveIndex(theIndex);
}

//
//	RemoveIndex - Remove the element with the given index from
//		the array. The element will NOT be deleted.
//
void CArray::RemoveIndex(int32 inIndex)
{
	CObject		*theObject;
	int32		i;
	
	if (ValidIndex(inIndex))
	{
		theObject = m_List[inIndex];
		
		for (i = inIndex; i < m_NumItems - 1; i++)
			m_List[i] = m_List[i + 1];
			
		m_NumItems--;
		
		m_List[m_NumItems] = NULL;
	}
}

//
//	RemoveAll - Remove all the elements of the array. This
//		will NOT delete the objects.
//
void CArray::RemoveAll(void)
{
	int32		i;
	
	for (i = 0; i < m_NumItems; i++)
		m_List[i] = NULL;
		
	m_NumItems = 0;
}

//
//	Delete - Remove the object then delete it.
//
void CArray::Delete(CObject *inObject)
{
	if (inObject != NULL)
	{
		Remove(inObject);
		delete inObject;
	}
}

//
//	DeleteIndex - Remove the object with the given index then
//		delete it.
//
void CArray::DeleteIndex(int32 inIndex)
{
	CObject		*theObject;
	
	if (ValidIndex(inIndex))
	{
		theObject = m_List[inIndex];
		
		RemoveIndex(inIndex);
		
		delete theObject;
	}
}

//
//	DeleteAll - Remove and delete all objects in the array.
//
void CArray::DeleteAll(void)
{
	CObject		*theObject;
	int32		i;
	
	for (i = 0; i < m_NumItems; i++)
	{
		theObject = m_List[i];
		if (theObject != NULL)
			delete theObject;
			
		m_List[i] = NULL;
	}
	
	m_NumItems = 0;
}
