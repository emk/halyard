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
// KDictionary.h : 
//

#if !defined (_KDictionary_h_)
#define _KDictionary_h_

#include "KCommon.h"
#include "KString.h"
#include "KDList.h"

template <class DataClass>
class KDictNode
{
public:
	KDictNode(KString &inKey, const DataClass &inData)
	{
		m_Key = inKey;
		m_Data = inData;
	}

	KString		m_Key;
	DataClass	m_Data;
};

template <class DataClass, int32 NumBuckets = 50>
class KDictionary
{
public:
					KDictionary();
	virtual			~KDictionary();

	void			Add(const KString &inKey, const DataClass &inData);
	void			Set(const KString &inKey, const DataClass &inData);
	bool			Find(const KString &inKey, DataClass &outData);
	int32			Count();

	bool			Remove(const KString &inKey, DataClass &outData);
	void			RemoveAll();

protected:

	TDList< KDictNode<DataClass> > m_Buckets[NumBuckets];
	int32			m_Count;

	int32			Hash(KString &inKey);
	int32			FindNode(const KString &inKey);
};

template <class DataClass, int32 NumBuckets>
KDictionary<DataClass, NumBuckets>::KDictionary()
{
	m_Count = 0;
}

template <class DataClass, int32 NumBuckets>
KDictionary<DataClass, NumBuckets>::~KDictionary()
{
	RemoveAll();
}


template <class DataClass, int32 NumBuckets>
void KDictionary<DataClass, NumBuckets>::Add(const KString &inKey, const DataClass &inData)
{
	int32	bucketNum = Find(inKey);
	if (bucketNum != -1)
		m_Buckets[bucketNum].Get().m_Data = inData;
	else
	{
		KDictNode<DataClass> newNode(inKey, inData);

		bucketNum = Hash(inKey);

		m_Buckets[bucketNum].AddTail(newNode);

		m_Count++;
	}
}

template <class DataClass, int32 NumBuckets>
void KDictionary<DataClass, NumBuckets>::Set(const KString &inKey, const DataClass &inData)
{
	int32	bucketNum = Find(inKey);

	if (bucketNum != -1)
		m_Buckets[bucketNum].Get().m_Data = inData;
}

template <class DataClass, int32 NumBuckets>
bool KDictionary<DataClass, NumBuckets>::Find(const KString &inKey, DataClass &outData)
{
	int32	bucketNum Find(inKey);
	bool	retValue = false;

	if (bucketNum != -1)
	{
		outData = m_Buckets[bucketNum].Get().m_Data;
		retValue = true;
	}

	return (retValue);
}

template <class DataClass, int32 NumBuckets>
inline int32 KDictionary<DataClass, NumBuckets>::Count()
{
	return (m_Count);
}

template <class DataClass, int32 NumBuckets>
bool KDictionary<DataClass, NumBuckets>::Remove(const KString &inKey, DataClass &outData)
{
	int32	bucketNum Find(inKey);
	bool	retValue = false;

	if (bucketNum != -1)
	{
		outData = m_Buckets[bucketNum].Get().m_Data;
		m_Buckets[bucketNum].Remove();

		m_Count--;

		retValue = true;
	}

	return (retValue);
}

template <class DataClass, int32 NumBuckets>
void KDictionary<DataClass, NumBuckets>::RemoveAll()
{

	for (int i = 0; i < NumBuckets; i++)
		m_Buckets[i].RemoveAll();

	m_Count = 0;
}

template <class DataClass, int32 NumBuckets>
int32 KDictionary<DataClass, NumBuckets>::FindNode(const KString &inKey)
{
	int32	bucketNum = Hash(inKey);

	if (m_Buckets[bucketNum].ToHead())
	{
		do
		{
			if (inKey == m_Buckets[bucketNum].Get().m_Key)
				return (bucketNum);
		} while (m_Buckets[bucketNum].ToNext());
	}

	return (-1);
}

template <class DataClass, int32 NumBuckets>
int32 KDictionary<DataClass, NumBuckets>::Hash(KString &inKey)
{
	const char	*strPtr = inKey.GeKString();
	uint32		hashVal = 0;

	while (*strPtr != '\0')
	{
		hashVal <<= 1;
		hashVal += uint32(*strPtr);
		strPtr++;
	}

	return (int32(hashVal % NumBuckets));
}

#endif // _KDictionary_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
