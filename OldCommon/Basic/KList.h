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
// KList.h : Singly-linked list class template.
//

#if !defined (_KList_h_)
#define _KList_h_

#include "KCommon.h"

template <class DataClass>
class KList
{
public:
				KList();
	virtual		~KList();

	int32		Count() { return (m_Count); }

	void		AddHead(const DataClass &inData);
	void		AddTail(const DataClass &inData);

	DataClass	GetNext();
	DataClass	Get();	
	void		Set(const DataClass &inData);

	bool		ToHead();
	bool		ToTail();
	bool		ToNext();

	bool		AtHead();
	bool		AtTail();

	void		RemoveHead();
	void		RemoveAll();

protected:
	struct KListNode
	{
		DataClass	m_Data;
		KListNode	*m_Next;
	};
	enum MoveDirection { MoveForward, MoveNot };

	int32			m_Count;
	KListNode		*m_Head;
	KListNode		*m_Tail;
	KListNode		*m_Current;

	KListNode		*CreateNode(DataClass inData);
	DataClass		GetObject(KListNode *inNode);
};

template <class DataClass>
KList<DataClass>::KList()
{
	m_Count = 0;
	m_Head = NULL;
	m_Tail = NULL;
	m_Current = NULL;
}

template <class DataClass>
KList<DataClass>::~KList()
{
	RemoveAll();
}

template <class DataClass>
void KList<DataClass>::AddHead(const DataClass &inData)
{
	KListNode	*newNode = CreateNode(inData);

	if (m_Head != NULL)
		newNode->m_Next = m_Head;

	m_Head = newNode;
	m_Current = m_Head;
	m_Count++;
}

template <class DataClass>
void KList<DataClass>::AddTail(const DataClass &inData)
{
	KListNode	*newNode = CreateNode(inData);

	if (m_Tail != NULL)
		m_Tail->m_Next = newNode;

	m_Tail = newNode;
	m_Current = m_Tail;
	m_Count++;
}

template <class DataClass>
DataClass KList<DataClass>::GetNext()
{
	return (GetObject(m_Current, MoveForward));
}

template <class DataClass>
DataClass KList<DataClass>::Get()
{
	return (GetObject(m_Current, MoveNot));
}

//
//	Set - Set the data for the current node.
//
template <class DataClass>
DataClass KList<DataClass>::Set(const DataClass &inData)
{
	if (m_Current != NULL)
		m_Current->m_Data = inData;
}

template <class DataClass>
bool KList<DataClass>::ToHead()
{
	bool	retValue = false;

	m_Current = m_Head;
	if (m_Current != NULL)
		retValue = true;

	return (retValue);
}

template <class DataClass>
bool KList<DataClass>::ToTail()
{
	bool	retValue = false;

	m_Current = m_Tail;
	if (m_Current != NULL)
		retValue = true;

	return (retValue);
}

template <class DataClass>
bool KList<DataClass>::AtHead()
{
	if ((m_Head != NULL) and (m_Current == m_Head))
		return (true);
	return (false);
}

template <class DataClass>
bool KList<DataClass>::AtTail()
{
	if ((m_Head != NULL) and (m_Current == m_Tail))
		return (true);
	return (false);
}

template <class DataClass>
void KList<DataClass>::RemoveHead()
{
	if (m_Head == NULL)
		return;

	KListNode	*delNode = m_Head;

	m_Head = m_Head->m_Next;
	m_Current = m_Head;

	if (m_Head == NULL)
		m_Tail = NULL;

	delete delNode;
	m_Count--;
}

template <class DataClass>
void KList<DataClass>::RemoveAll()
{
	if (m_Head == NULL)
		return;

	ToHead();
	while (m_Head != NULL)
		RemoveHead();
}

template <class DataClass>
KList<DataClass::KListNode *KList<DataClass>::CreateNode(DataClass inData)
{
	KListNode	*newNode = new KListNode;

	// throw exception if out of memory
	ASSERT(newNode != NULL);

	newNode->m_Data = inData;
	newNode->m_Next = NULL;

	return (newNode);
}

template <class DataClass>
DataClass KList<DataClass>::GetObject(KListNode *inNode, MoveDirection inDir)
{
	if (inNode == NULL)
		return (NULL);

	DataClass	retObject = inNode->m_Data;

	if (inDir == MoveForward)
		m_Current = inNode->m_Next;

	return (retObject);
}


#endif // _KList_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
