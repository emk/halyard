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

void DatumMap::RemoveKnownDatum(const std::string &inKey, Datum *inDatum)
{
	// This method removes a Datum known to be in the map, without deleting
	// it.  This is generally called from Apply and Revert methods.
	DatumMap::iterator found = find(inKey);
	ASSERT(found != end());
	ASSERT(found->second == inDatum);
	erase(found);
}

DatumList::~DatumList()
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
//  MapDatum Methods
//=========================================================================

class MapDatum::SetChange : public Change {
	MapDatum *mMapDatum;
	Datum *mOldDatum;
	Datum *mNewDatum;
	MapDatum::ConstKeyType mKey;
	
public:
	SetChange(MapDatum *inDatum,
			  MapDatum::ConstKeyType &inKey,
			  Datum *inValue);
	
protected:
	virtual void DoApply();
	virtual void DoRevert();
	virtual void DoFreeApplyResources();
	virtual void DoFreeRevertResources();
};

MapDatum::SetChange::SetChange(MapDatum *inDatum,
							   MapDatum::ConstKeyType &inKey,
							   Datum *inValue)
	: mMapDatum(inDatum), mOldDatum(NULL), mNewDatum(inValue), mKey(inKey)
{
	DatumMap::iterator found = mMapDatum->mMap.find(inKey);
	if (found != mMapDatum->mMap.end())
		mOldDatum = found->second;
}

void MapDatum::SetChange::DoApply()
{
	if (mOldDatum)
		mMapDatum->mMap.RemoveKnownDatum(mKey, mOldDatum);
	mMapDatum->mMap.insert(DatumMap::value_type(mKey, mNewDatum));
}

void MapDatum::SetChange::DoRevert()
{
	mMapDatum->mMap.RemoveKnownDatum(mKey, mNewDatum);
	if (mOldDatum)
		mMapDatum->mMap.insert(DatumMap::value_type(mKey, mOldDatum));
}

void MapDatum::SetChange::DoFreeApplyResources()
{
	delete mNewDatum;
	mNewDatum = NULL;
}

void MapDatum::SetChange::DoFreeRevertResources()
{
	if (mOldDatum)
	{
		delete mOldDatum;
		mOldDatum = NULL;
	}
}

void MapDatum::DoSet(ConstKeyType &inKey, Datum *inValue)
{
	RegisterChildObjectWithStore(inValue);
	ApplyChange(new MapDatum::SetChange(this, inKey, inValue));
}

Datum *MapDatum::DoGet(ConstKeyType &inKey)
{
	DatumMap::iterator found = mMap.find(inKey);
	if (found == mMap.end())
		throw TException(__FILE__, __LINE__, "MapDatum::Get: Can't find key");
	return found->second;
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
