// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TCommon.h"
#include "DataStore.h"

using namespace DataStore;
using namespace DataStore::Private;


//=========================================================================
//  Private Data Types
//=========================================================================

DatumMap::~DatumMap()
{
	for (iterator i = begin(); i != end(); i++)
		delete i->second;
}

DatumVector::~DatumVector()
{
	for (iterator i = begin(); i != end(); i++)
		delete *i;	
}


//=========================================================================
//  Change Methods
//=========================================================================

Change::Change()
	: mIsApplied(false), mIsFreed(false)
{
}

Change::~Change()
{
	ASSERT(mIsFreed);
}

void Change::Apply()
{
	ASSERT(mIsFreed == false);
	ASSERT(mIsApplied == false);
	DoApply();
	mIsApplied = true;
}

void Change::Revert()
{
	ASSERT(mIsFreed == false);
	ASSERT(mIsApplied == true);
	DoRevert();
	mIsApplied = false;
}

void Change::FreeResources()
{
	// This function can't be part of the destructor because destructors
	// apparently can't call virtual functions.
	ASSERT(mIsFreed == false);
	if (mIsApplied)
		DoFreeRevertResources();
	else
		DoFreeApplyResources();
	mIsFreed = true;
}


//=========================================================================
//  MutableDatum Methods
//=========================================================================

void MutableDatum::ApplyChange(Change *inChange)
{
	ASSERT(mStore != NULL);
	ASSERT(inChange != NULL);
	mStore->ApplyChange(inChange);
}

void MutableDatum::RegisterChildObjectWithStore(Datum *inDatum)
{
	ASSERT(mStore != NULL);
	ASSERT(inDatum != NULL);
	inDatum->RegisterWithStore(mStore);
}

void MutableDatum::RegisterWithStore(Store *inStore)
{
	ASSERT(mStore == NULL);
	ASSERT(inStore != NULL);
	mStore = inStore;
}


//=========================================================================
//  CollectionDatum Methods
//=========================================================================
//  Supported types of changes
//    MapDatum container
//      Set
//        mContainer
//        mKey
//        mOldDatum (optional) *
//        mNewDatum
//        find(key) -> datum-or-null
//        remove-known-datum(key, datum)
//        insert(key, datum)
//      Delete
//        mContainer
//        mKey
//        mOldDatum *
//        find(key) -> datum-or-null
//        remove-known-datum(key, datum)
//        insert(key, datum)
//    ListDatum container
//      Set
//        mContainer
//        mKey
//        mOldDatum (optional) *
//        mNewDatum
//        find(key) -> datum-or-null
//        remove-known-datum(key, datum)
//        insert(key, datum)
//      Insert
//        mContainer
//        mKey
//        mNewDatum
//        insert(key, datum)
//        remove-known-datum(key, datum)
//      Delete
//        mContainer
//        mKey
//        mOldDatum *
//        find(key) -> datum-or-null
//        remove-known-datum(key, datum)
//        insert(key, datum)
//    Multiple containers
//      Transfer
//        mOldContainer
//        mOldKey
//        mNewContainer
//        mNewKey
//        mOldDatum (optional) *
//        mNewDatum
//        find(key) -> datum-or-null
//        remove-known-datum(key, datum)
//        insert(key, datum)


//=========================================================================
//  CollectionDatum::SetChange
//=========================================================================

template <typename KeyType>
class CollectionDatum<KeyType>::SetChange : public Change {
	CollectionDatum<KeyType> *mCollection;
	Datum *mOldDatum;
	Datum *mNewDatum;
	ConstKeyType mKey;

public:
	SetChange(CollectionDatum<KeyType> *inDatum,
			  ConstKeyType &inKey,
			  Datum *inValue);

protected:
	virtual void DoApply();
	virtual void DoRevert();
	virtual void DoFreeApplyResources();
	virtual void DoFreeRevertResources();
};

template <typename KeyType>
CollectionDatum<KeyType>::
SetChange::SetChange(CollectionDatum<KeyType> *inDatum,
					 ConstKeyType &inKey,
					 Datum *inValue)
	: mCollection(inDatum), mOldDatum(NULL), mNewDatum(inValue), mKey(inKey)
{
	mOldDatum = mCollection->DoFind(inKey);
}

template <typename KeyType>
void CollectionDatum<KeyType>::SetChange::DoApply()
{
	if (mOldDatum)
		mCollection->DoRemoveKnown(mKey, mOldDatum);
	mCollection->DoInsert(mKey, mNewDatum);
}

template <typename KeyType>
void CollectionDatum<KeyType>::SetChange::DoRevert()
{
	mCollection->DoRemoveKnown(mKey, mNewDatum);
	if (mOldDatum)
		mCollection->DoInsert(mKey, mOldDatum);
}

template <typename KeyType>
void CollectionDatum<KeyType>::SetChange::DoFreeApplyResources()
{
	delete mNewDatum;
	mNewDatum = NULL;
}

template <typename KeyType>
void CollectionDatum<KeyType>::SetChange::DoFreeRevertResources()
{
	if (mOldDatum)
	{
		delete mOldDatum;
		mOldDatum = NULL;
	}
}

template <typename KeyType>
void CollectionDatum<KeyType>::PerformSet(ConstKeyType &inKey, Datum *inValue)
{
	RegisterChildObjectWithStore(inValue);
	ApplyChange(new SetChange(this, inKey, inValue));
}


//=========================================================================
//  CollectionDatum Instantiation
//=========================================================================

template class CollectionDatum<std::string>;
template class CollectionDatum<size_t>;


//=========================================================================
//  MapDatum Methods
//=========================================================================

Datum *MapDatum::DoGet(ConstKeyType &inKey)
{
	DatumMap::iterator found = mMap.find(inKey);
	if (found == mMap.end())
		throw TException(__FILE__, __LINE__, "MapDatum::Get: Can't find key");
	return found->second;
}

Datum *MapDatum::DoFind(ConstKeyType &inKey)
{
	DatumMap::iterator found = mMap.find(inKey);
	if (found == mMap.end())
		return NULL;
	return found->second;
}

void MapDatum::DoRemoveKnown(ConstKeyType &inKey, Datum *inDatum)
{
	DatumMap::iterator found = mMap.find(inKey);
	ASSERT(found != mMap.end());
	ASSERT(found->second == inDatum);
	mMap.erase(found);
}

void MapDatum::DoInsert(ConstKeyType &inKey, Datum *inDatum)
{
	mMap.insert(DatumMap::value_type(inKey, inDatum));
}


//=========================================================================
//  ListDatum::InsertChange
//=========================================================================

class ListDatum::InsertChange : public Change {
	ListDatum *mCollection;
	Datum *mNewDatum;
	ConstKeyType mKey;

public:
	InsertChange(ListDatum *inCollection,
				 ConstKeyType &inKey,
				 Datum *inValue);

protected:
	virtual void DoApply();
	virtual void DoRevert();
	virtual void DoFreeApplyResources();
	virtual void DoFreeRevertResources();
};

ListDatum::InsertChange::InsertChange(ListDatum *inCollection,
									  ConstKeyType &inKey,
									  Datum *inValue)
	: mCollection(inCollection), mNewDatum(inValue), mKey(inKey)
{
}

void ListDatum::InsertChange::DoApply()
{
	mCollection->DoInsert(mKey, mNewDatum);
}

void ListDatum::InsertChange::DoRevert()
{
	mCollection->DoRemoveKnown(mKey, mNewDatum);
}

void ListDatum::InsertChange::DoFreeApplyResources()
{
	delete mNewDatum;
	mNewDatum = NULL;	
}

void ListDatum::InsertChange::DoFreeRevertResources()
{
}

void ListDatum::PerformInsert(ConstKeyType &inKey, Datum *inValue)
{
	RegisterChildObjectWithStore(inValue);
	ApplyChange(new InsertChange(this, inKey, inValue));
}


//=========================================================================
//  ListDatum Methods
//=========================================================================

Datum *ListDatum::DoGet(ConstKeyType &inKey)
{
	if (inKey >= mVector.size())
		throw TException(__FILE__, __LINE__,
						 "No such key in ListDatum::DoGet");
	return mVector[inKey];
}

Datum *ListDatum::DoFind(ConstKeyType &inKey)
{
	if (inKey < mVector.size())
		return mVector[inKey];
	else
		return NULL;
}

void ListDatum::DoRemoveKnown(ConstKeyType &inKey, Datum *inDatum)
{
	// This runs in O(N) time, which isn't great.
	ASSERT(inKey < mVector.size());
	ASSERT(inDatum != NULL);
	ASSERT(mVector[inKey] == inDatum);
	mVector.erase(mVector.begin() + inKey);
}

void ListDatum::DoInsert(ConstKeyType &inKey, Datum *inDatum)
{
	// This runs in O(N) time, which isn't great.
	ASSERT(inKey <= mVector.size());
	ASSERT(inDatum != NULL);
	mVector.insert(mVector.begin() + inKey, inDatum);
}


//=========================================================================
//  Store Methods
//=========================================================================

Store::Store()
	: mRoot(NULL)
{
	mChangePosition = mChanges.begin();
	mRoot = new MapDatum();
	mRoot->RegisterWithStore(this);
}

Store::~Store()
{
	ClearRedoList();
	ClearUndoList();
}

bool Store::CanUndo()
{
	return mChangePosition != mChanges.begin();
}

void Store::Undo()
{
	ASSERT(CanUndo());
	(*--mChangePosition)->Revert();
}

void Store::ClearUndoList()
{
	ChangeList::iterator i = mChanges.begin();
	for (; i != mChangePosition; i = mChanges.erase(i))
	{
		(*i)->FreeResources();
		delete *i;
	}
	ASSERT(!CanUndo());
}

bool Store::CanRedo()
{
	return mChangePosition != mChanges.end();
}

void Store::Redo()
{
	ASSERT(CanRedo());
	(*mChangePosition++)->Apply();
}

void Store::ClearRedoList()
{
	// If there's no redo list, give up now.
	if (!CanRedo())
		return;

	// Walk the redo list backwards, destroying the Change objects
	// pointed to by each list element.
	ChangeList::iterator i = mChanges.end();
	do
	{
		--i;
		(*i)->FreeResources();
		delete *i;
		*i = NULL;
	} while (i != mChangePosition);

	// Free the actual list elements.
	mChangePosition = mChanges.erase(mChangePosition, mChanges.end());
	ASSERT(!CanRedo());	
}

void Store::ApplyChange(Change *inChange)
{
	ClearRedoList();
	inChange->Apply();
	mChangePosition = mChanges.insert(mChangePosition, inChange);
	mChangePosition++;
}
