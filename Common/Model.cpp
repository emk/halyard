// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <boost/lexical_cast.hpp>
#include <libxml/parser.h>

#include "TCommon.h"
#include "DataStore.h"

USING_NAMESPACE_FIVEL
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
//  ValueDatum Implementation
//=========================================================================


void IntegerDatum::Write(xml_node inParent)
{
	inParent.new_child("int", boost::lexical_cast<std::string>(mValue));
}

void StringDatum::Write(xml_node inParent)
{
	inParent.new_child("str", mValue);
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
//  CollectionDatum::DeleteChange
//=========================================================================

template <typename KeyType>
class CollectionDatum<KeyType>::DeleteChange : public Change
{
	CollectionDatum<KeyType> *mCollection;
	Datum *mOldDatum;
	ConstKeyType mKey;

public:
	DeleteChange(CollectionDatum<KeyType> *inDatum,
				 ConstKeyType &inKey);

protected:
	virtual void DoApply();
	virtual void DoRevert();
	virtual void DoFreeApplyResources();
	virtual void DoFreeRevertResources();
};

template <typename KeyType>
CollectionDatum<KeyType>::DeleteChange::DeleteChange(
	CollectionDatum<KeyType> *inDatum, ConstKeyType &inKey)
	: mCollection(inDatum), mKey(inKey)
{
	mOldDatum = mCollection->DoFind(inKey);
	if (!mOldDatum)
	{
		ConstructorFailing();
		THROW("DeleteChange: No such key");
	}
}

template <typename KeyType>
void CollectionDatum<KeyType>::DeleteChange::DoApply()
{
	mCollection->DoRemoveKnown(mKey, mOldDatum);
}

template <typename KeyType>
void CollectionDatum<KeyType>::DeleteChange::DoRevert()
{
	mCollection->DoInsert(mKey, mOldDatum);
}

template <typename KeyType>
void CollectionDatum<KeyType>::DeleteChange::DoFreeApplyResources()
{
}

template <typename KeyType>
void CollectionDatum<KeyType>::DeleteChange::DoFreeRevertResources()
{
	delete mOldDatum;
	mOldDatum = NULL;
}

template <typename KeyType>
void CollectionDatum<KeyType>::PerformDelete(ConstKeyType &inKey)
{
	ApplyChange(new DeleteChange(this, inKey));
}


//=========================================================================
//  CollectionDatum Instantiation
//=========================================================================

template class CollectionDatum<std::string>;
template class CollectionDatum<size_t>;


//=========================================================================
//  MapDatum Methods
//=========================================================================

void MapDatum::Write(xml_node inParent)
{
	xml_node node = inParent.new_child("map");
	DatumMap::iterator i = mMap.begin();
	for (; i != mMap.end(); ++i)
	{
		xml_node item = node.new_child("item");
		item.set_attribute("key", i->first);
		i->second->Write(item);
	}
}

void MapDatum::Fill(xml_node inNode)
{
	xml_node::iterator i = inNode.begin();
	for (; i != inNode.end(); ++i)
	{
		xml_node node = *i;
		XML_CHECK_NAME(node, "item");
		std::string key = node.attribute("key");
		//xml_node value = node.only_child();
	}
}

Datum *MapDatum::DoGet(ConstKeyType &inKey)
{
	DatumMap::iterator found = mMap.find(inKey);
	CHECK(found != mMap.end(), "MapDatum::Get: Can't find key");
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

void ListDatum::Write(xml_node inParent)
{
	xml_node node = inParent.new_child("list");
	DatumVector::iterator i = mVector.begin();
	for (; i != mVector.end(); ++i)
		(*i)->Write(node);
}

Datum *ListDatum::DoGet(ConstKeyType &inKey)
{
	CHECK(inKey < mVector.size(), "No such key in ListDatum::DoGet");
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
//  MoveChange
//=========================================================================

#if 0
template <typename DestKeyType, typename SrcKeyType>
class MoveChange : public Change {
	typedef CollectionDatum<DestKeyType> DestCollection;
	typedef CollectionDatum<SrcKeyType> SrcCollection;
	typedef DestCollection::ConstKeyType DestConstKeyType;
	typedef SrcCollection::ConstKeyType SrcConstKeyType;

	SrcCollection *mOldCollection;
	SrcConstKeyType mOldKey;
	DestCollection *mNewCollection;
	DestConstKeyType mNewKey;
	Datum *mReplacedDatum;
	Datum *mMovedDatum;

public:
	MoveChange(DestCollection *inDest, DestConstKeyType &inDestKey,
			   SrcCollection *inSrc, SrcConstKeyType &inSrcKey);

protected:
	virtual void DoApply();
	virtual void DoRevert();
	virtual void DoFreeApplyResources();
	virtual void DoFreeRevertResources();
};

template <typename DestKeyType, typename SrcKeyType>
MoveChange<DestKeyType,SrcKeyType>::MoveChange(DestCollection *inDest,
											   DestConstKeyType &inDestKey,
											   SrcCollection *inSrc,
											   SrcConstKeyType &inSrcKey)
	: mOldCollection(inSrc), mOldKey(inSrcKey),
	  mNewCollection(inDest), mNewKey(inDestKey),
	  mReplacedDatum(NULL), mMovedDatum(NULL)
{
	mReplacedDatum = mNewCollection->DoFind(mNewKey);
	mMovedDatum = mOldCollection->DoFind(mOldKey);
	if (!mMovedDatum)
	{
		ConstructorFailing();
		THROW("DeleteChange: No such key");
	}
}

template <typename DestKeyType, typename SrcKeyType>
void MoveChange<DestKeyType,SrcKeyType>::DoApply()
{
	if (mReplacedDatum)
		mNewCollection->DoRemoveKnown(mNewKey, mReplacedDatum);
	mOldCollection->DoRemoveKnown(mOldKey, mMovedDatum);
	mNewCollection->DoInsert(mNewKey, mMovedDatum);
}

template <typename DestKeyType, typename SrcKeyType>
void MoveChange<DestKeyType,SrcKeyType>::DoRevert()
{
	mNewCollection->DoRemoveKnown(mNewKey, mMovedDatum);
	mOldCollection->DoInsert(mOldKey, mMovedDatum);
	if (mReplacedDatum)
		mNewCollection->DoInsert(mNewKey, mReplacedDatum);
}

template <typename DestKeyType, typename SrcKeyType>
void MoveChange<DestKeyType,SrcKeyType>::DoFreeApplyResources()
{
}

template <typename DestKeyType, typename SrcKeyType>
void MoveChange<DestKeyType,SrcKeyType>::DoFreeRevertResources()
{
	if (mReplacedDatum)
	{
		delete mReplacedDatum;
		mReplacedDatum = NULL;
	}
}

template <typename C1, typename C2>
void DataStore::Move(C1 *inDest, typename C1::ConstKeyType &inDestKey,
					 C2 *inSrc, typename C2::ConstKeyType &inSrcKey)
{
	inDest->GetStore()->
      ApplyChange(new MoveChange<typename C1::KeyType,
				                 typename C2::KeyType>(inDest, inDestKey,
													   inSrc, inSrcKey));
}

template MoveChange<size_t,size_t>;
template MoveChange<std::string,size_t>;
template MoveChange<size_t,std::string>;
template MoveChange<std::string,std::string>;

template void DataStore::Move(ListDatum*, ListDatum::ConstKeyType&,
							  ListDatum*, ListDatum::ConstKeyType&);

template void DataStore::Move(MapDatum*, MapDatum::ConstKeyType&,
							  MapDatum*, MapDatum::ConstKeyType&);
#endif // 0


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

void Store::Write(const std::string &inFile)
{
	// Create a tree.
	xmlDocPtr doc = xmlNewDoc(xml_node::to_utf8("1.0"));
	doc->children =
		xmlNewDocNode(doc, NULL, xml_node::to_utf8("TamaleData"), NULL);
	xml_node root(doc->children);
	root.set_attribute("version", "0");
	root.set_attribute("backto", "0");

	// Add nodes as appropriate.
	mRoot->Write(root);

	// Serialize it to a file.
	int result = xmlSaveFormatFile(inFile.c_str(), doc, 1);
	xmlFreeDoc(doc);
	CHECK(result != -1, "Failed to save XML file");
}

Store *Store::Read(const std::string &inFile)
{
	// Create a new data store.
	std::auto_ptr<Store> store(new Store());

	// Open the XML file.
	xmlDocPtr doc = xmlParseFile(inFile.c_str());
	CHECK(doc, "Failed to load XML file");
	try
	{
		// Check out the root element.
		// XXX - Version check goes here.
		xmlNodePtr root = xmlDocGetRootElement(doc);
		CHECK(root, "No document root in XML file");
		xml_node map_node = xml_node(root).only_child();
		XML_CHECK_NAME(map_node, "map");
		store->GetRoot()->Fill(map_node);
	}
	catch (...)
	{
		xmlFreeDoc(doc);
		throw;
	}

	xmlFreeDoc(doc);
	store->ClearUndoList();
	return store.release();
}
