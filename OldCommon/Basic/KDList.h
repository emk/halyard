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
// KDList.h : Doubly-linked list class template.
//

#if !defined (_KDList_h_)
#define _KDList_h_

#include "KCommon.h"

template <class DataClass>
class KDList
{
public:
				KDList();
	virtual		~KDList();

	int32		Count() { return (m_Count); }

	void		AddHead(const DataClass &inData);
	void		AddTail(const DataClass &inData);
	void		AddBefore(const DataClass &inData);
	void		AddAfter(const DataClass &inData);

	DataClass	GetNext();
	DataClass	GetPrev();
	DataClass	Get();
	void		Set(const DataClass &inData);

	bool		ToHead();
	bool		ToTail();
	bool		ToNext();
	bool		ToPrev();

	bool		AtHead();
	bool		AtTail();

	void		RemoveHead();
	void		RemoveTail();
	void		Remove();
	void		RemoveAll();

protected:
	struct KDListNode
	{
		DataClass	m_Data;
		KDListNode	*m_Next;
		KDListNode	*m_Prev;
	};

	enum MoveDirection { MoveForward, MoveBackward, MoveNot };

	int32			m_Count;
	KDListNode		*m_Head;
	KDListNode		*m_Tail;
	KDListNode		*m_Current;

	KDListNode		*CreateNode(DataClass inData);
	DataClass		GetObject(KDListNode *inNode, MoveDirection inDir);
};

template <class DataClass>
KDList<DataClass>::KDList()
{
	m_Count = 0;
	m_Head = NULL;
	m_Tail = NULL;
	m_Current = NULL;
}

template <class DataClass>
KDList<DataClass>::~KDList()
{
	RemoveAll();
}

template <class DataClass>
void KDList<DataClass>::AddHead(const DataClass &inData)
{
	KDListNode	*newNode = CreateNode(inData);

	if (m_Head != NULL)
		newNode->m_Next = m_Head;
	else
		m_Tail = newNode;

	m_Head = newNode;

	m_Current = newNode;
	m_Count++;
}

template <class DataClass>
void KDList<DataClass>::AddTail(const DataClass &inData)
{
	KDListNode *newNode = CreateNode(inData);

	if (m_Tail != NULL)
	{
		m_Tail->m_Next = newNode;
		newNode->m_Prev = m_Tail;
	}
	else
		m_Head = newNode;

	m_Tail = newNode;

	m_Current = newNode;
	m_Count++;
}

template <class DataClass>
void KDList<DataClass>::AddBefore(const DataClass &inData)
{
	KDListNode *newNode = CreateNode(inData);

	if (m_Current != NULL)
	{
		newNode->m_Prev = m_Current->m_Prev;
		newNode->m_Next = m_Current;

		m_Current->m_Prev = newNode;
		if (m_Current->m_Prev != NULL)
			m_Current->m_Prev->m_Next = newNode;
	}
	else
	{
		m_Head = newNode;
		m_Tail = newNode;
	}

	m_Current = newNode;
	m_Count++;
}

template <class DataClass>
void KDList<DataClass>::AddAfter(const DataClass &inData)
{
	KDListNode *newNode = CreateNode(inData);

	if (m_Current != NULL)
	{
		newNode->m_Prev = m_Current;
		newNode->m_Next = m_Current->m_Next;

		m_Current->m_Next = newNode;
		if (m_Current->m_Next != NULL)
			m_Current->m_Next->m_Prev = newNode;
	}
	else
	{
		m_Head = newNode;
		m_Tail = newNode;
	}

	m_Current = newNode;
	m_Count++;
}

template <class DataClass>
DataClass KDList<DataClass>::GetNext()
{
	return (GetObject(m_Current, MoveForward));
}

template <class DataClass>
DataClass KDList<DataClass>::GetPrev()
{
	return (GetObject(m_Current, MoveBackward));
}

template <class DataClass>
DataClass KDList<DataClass>::Get()
{
	return (GetObject(m_Current, MoveNot));
}

//
//	Set - Set the data for the current node. This will
//		NOT delete the previous object.
//
template <class DataClass>
void KDList<DataClass>::Set(const DataClass &inData)
{
	if (m_Current != NULL)
		m_Current->m_Data = inData;
}

template <class DataClass>
bool KDList<DataClass>::ToHead()
{
	bool	retValue = false;

	m_Current = m_Head;
	if (m_Current != NULL)
		retValue = true;

	return (retValue);
}

template <class DataClass>
bool KDList<DataClass>::ToTail()
{
	bool	retValue = false;

	m_Current = m_Tail;
	if (m_Current != NULL)
		retValue = true;
}

template <class DataClass>
bool KDList<DataClass>::ToNext()
{
	bool	retValue = false;

	if (m_Current != NULL)
	{
		m_Current = m_Current->m_Next;
		if (m_Current != NULL)
			retValue = true;
	}

	return (retValue)
}

template <class DataClass>
bool KDList<DataClass>::ToPrev()
{
	bool	retValue = false;

	if (m_Current != NULL)
	{
		m_Current = m_Current->m_Prev;
		if (m_Current != NULL)
			retValue = true;
	}

	return (retValue);
}

template <class DataClass>
bool KDList<DataClass>::AtHead()
{
	if ((m_Head != NULL) and (m_Current == m_Head))
		return (true);
	return (false);
}

template <class DataClass>
bool KDList<DataClass>::AtTail()
{
	if ((m_Head != NULL) and (m_Current == m_Tail))
		return (true);
	return (false);
}

template <class DataClass>
void KDList<DataClass>::RemoveHead()
{
	if (m_Head == NULL)
		return;

	KDListNode	*delNode = m_Head;

	if (m_Head->m_Next != NULL)
		m_Head->m_Next->m_Prev = NULL;

	m_Head = m_Head->m_Next;
	m_Current = m_Head;

	if (m_Head == NULL)
		m_Tail = NULL;

	delete delNode;
	m_Count--;
}

template <class DataClass>
void KDList<DataClass>::RemoveTail()
{
	if (m_Tail == NULL)
		return;

	KDListNode	*delNode = m_Tail;

	if (m_Tail->m_Prev != NULL)
		m_Tail->m_Prev->m_Next = NULL;

	m_Tail = m_Tail->m_Prev;
	m_Current = m_Tail;

	if (m_Tail == NULL)
		m_Head = NULL;

	delete delNode;
	m_Count--;
}

template <class DataClass>
void KDList<DataClass>::Remove()
{
	if (m_Current == NULL)
		return;

	KDListNode	*delNode = m_Current;

	if (m_Current->m_Next != NULL)
		m_Current->m_Next->m_Prev = m_Current->m_Prev;

	if (m_Current->m_Prev != NULL)
		m_Current->m_Prev->m_Next = m_Current->m_Next;

	m_Current = m_Current->m_Next;

	// see if we were at the tail
	if (m_Current == NULL)
	{
		m_Tail = delNode->m_Prev;

		// see if we at the head too
		if (m_Tail == NULL)
		{
			m_Head = NULL;
			m_Current = NULL;
		}
		else
			m_Current = m_Tail;
	}

	// see if we were at the head
	if (m_Head == delNode)
		m_Head = m_Current;

	delete delNode;
	m_Count--;
}

template <class DataClass>
void KDList<DataClass>::RemoveAll()
{
	if (m_Head == NULL)
		return;

	ToHead();
	while (m_Head != NULL)
		RemoveHead();
}

//
//	CreateNode - 
//
template <class DataClass>
KDList<DataClass>::KDListNode *KDList<DataClass>::CreateNode(DataClass inData)
{
	KDListNode *newNode = new KDListNode;
	// throw exception if out of memory
	ASSERT(newNode != NULL);

	newNode->m_Data = inData;
	newNode->m_Next = NULL;
	newNode->m_Prev = NULL;

	return (newNode);
}

//
//	GetObject - Given a node return its object and update the
//		current pointer to point to the next node.
//
template <class DataClass>
DataClass KDList<DataClass>::GetObject(KDListNode *inNode, MoveDirection inDir)
{
	if (inNode == NULL)
		return (NULL);

	DataClass retObject = inNode->m_Data;

	if (inDir == MoveForward)
		m_Current = inNode->m_Next;
	else if (inDir == MoveBackward)
		m_Current = inNode->m_Prev;

	return (retObject);
}
#endif // _KDList_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
