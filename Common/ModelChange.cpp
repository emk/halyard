// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TCommon.h"
#include "Model.h"
#include "ModelChange.h"

USING_NAMESPACE_FIVEL
using namespace model;


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
//  SetChange Methods
//=========================================================================

template <typename KeyType>
SetChange<KeyType>::SetChange(CollectionDatum<KeyType> *inDatum,
							  ConstKeyType &inKey,
							  Datum *inValue)
	: mCollection(inDatum), mOldDatum(NULL), mNewDatum(inValue), mKey(inKey)
{
	mOldDatum = Find(mCollection, inKey);
}

template <typename KeyType>
void SetChange<KeyType>::DoApply()
{
	if (mOldDatum)
		RemoveKnown(mCollection, mKey, mOldDatum);
	Insert(mCollection, mKey, mNewDatum);
	NotifyChanged(mCollection);
	if (mOldDatum)
		NotifyDeleted(mOldDatum);
	NotifyUndeleted(mNewDatum);
}

template <typename KeyType>
void SetChange<KeyType>::DoRevert()
{
	RemoveKnown(mCollection, mKey, mNewDatum);
	if (mOldDatum)
		Insert(mCollection, mKey, mOldDatum);
	NotifyChanged(mCollection);
	NotifyDeleted(mNewDatum);
	if (mOldDatum)
		NotifyUndeleted(mOldDatum);
}

template <typename KeyType>
void SetChange<KeyType>::DoFreeApplyResources()
{
	delete mNewDatum;
	mNewDatum = NULL;
}

template <typename KeyType>
void SetChange<KeyType>::DoFreeRevertResources()
{
	if (mOldDatum)
	{
		delete mOldDatum;
		mOldDatum = NULL;
	}
}

template class model::SetChange<std::string>;
template class model::SetChange<size_t>;


//=========================================================================
//  DeleteChange Methods
//=========================================================================

template <typename KeyType>
DeleteChange<KeyType>::DeleteChange(
	CollectionDatum<KeyType> *inDatum, ConstKeyType &inKey)
	: mCollection(inDatum), mKey(inKey)
{
	mOldDatum = Find(mCollection, inKey);
	if (!mOldDatum)
	{
		ConstructorFailing();
		THROW("DeleteChange: No such key");
	}
}

template <typename KeyType>
void DeleteChange<KeyType>::DoApply()
{
	RemoveKnown(mCollection, mKey, mOldDatum);
	NotifyChanged(mCollection);
	NotifyDeleted(mOldDatum);
}

template <typename KeyType>
void DeleteChange<KeyType>::DoRevert()
{
	Insert(mCollection, mKey, mOldDatum);
	NotifyChanged(mCollection);
	NotifyUndeleted(mOldDatum);
}

template <typename KeyType>
void DeleteChange<KeyType>::DoFreeApplyResources()
{
}

template <typename KeyType>
void DeleteChange<KeyType>::DoFreeRevertResources()
{
	delete mOldDatum;
	mOldDatum = NULL;
}

template class model::DeleteChange<std::string>;
template class model::DeleteChange<size_t>;


//=========================================================================
//  InsertChange
//=========================================================================

InsertChange::InsertChange(List *inCollection,
						   ConstKeyType &inKey,
						   Datum *inValue)
	: mCollection(inCollection), mNewDatum(inValue), mKey(inKey)
{
}

void InsertChange::DoApply()
{
	Insert(mCollection, mKey, mNewDatum);
	NotifyChanged(mCollection);
	NotifyUndeleted(mNewDatum);
}

void InsertChange::DoRevert()
{
	RemoveKnown(mCollection, mKey, mNewDatum);
	NotifyChanged(mCollection);
	NotifyDeleted(mNewDatum);
}

void InsertChange::DoFreeApplyResources()
{
	delete mNewDatum;
	mNewDatum = NULL;	
}

void InsertChange::DoFreeRevertResources()
{
}
