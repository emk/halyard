// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <boost/lexical_cast.hpp>
#include <libxml/parser.h>

#include "TCommon.h"
#include "Model.h"

USING_NAMESPACE_FIVEL
using namespace model;
using namespace model::Private;
using boost::lexical_cast;


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
//  Datum Methods
//=========================================================================

Datum *Datum::CreateFromXML(xml_node inNode)
{
	std::string name  = inNode.name();
	if (name == "int")
		return new Integer(lexical_cast<long>(inNode.text()));
	else if (name == "str")
		return new String(inNode.text());
	else if (name == "map")
		return new Map();
	else if (name == "list")
		return new List();
	else
		THROW("Unsupported XML element type");
}


//=========================================================================
//  ValueDatum Implementation
//=========================================================================

void Integer::Write(xml_node inParent)
{
	inParent.new_child("int", lexical_cast<std::string>(mValue));
}

void String::Write(xml_node inParent)
{
	inParent.new_child("str", mValue);
}


//=========================================================================
//  MutableDatum Methods
//=========================================================================

void MutableDatum::ApplyChange(Change *inChange)
{
	ASSERT(mModel != NULL);
	ASSERT(inChange != NULL);
	mModel->ApplyChange(inChange);
}

void MutableDatum::RegisterChildObjectWithModel(Datum *inDatum)
{
	ASSERT(mModel != NULL);
	ASSERT(inDatum != NULL);
	inDatum->RegisterWithModel(mModel);
}

void MutableDatum::RegisterWithModel(Model *inModel)
{
	ASSERT(mModel == NULL);
	ASSERT(inModel != NULL);
	mModel = inModel;
}


//=========================================================================
//  CollectionDatum Methods
//=========================================================================
//  Supported types of changes
//    Map container
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
//    List container
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
	RegisterChildObjectWithModel(inValue);
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
//  Map Methods
//=========================================================================

void Map::Write(xml_node inParent)
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

void Map::Fill(xml_node inNode)
{
	xml_node::iterator i = inNode.begin();
	for (; i != inNode.end(); ++i)
	{
		xml_node node = *i;
		XML_CHECK_NAME(node, "item");
		std::string key = node.attribute("key");
		xml_node value_node = node.only_child();
		Datum *value = CreateFromXML(value_node);
		Set(key, value);
		value->Fill(value_node);
	}
}

Datum *Map::DoGet(ConstKeyType &inKey)
{
	DatumMap::iterator found = mMap.find(inKey);
	CHECK(found != mMap.end(), "Map::Get: Can't find key");
	return found->second;
}

Datum *Map::DoFind(ConstKeyType &inKey)
{
	DatumMap::iterator found = mMap.find(inKey);
	if (found == mMap.end())
		return NULL;
	return found->second;
}

void Map::DoRemoveKnown(ConstKeyType &inKey, Datum *inDatum)
{
	DatumMap::iterator found = mMap.find(inKey);
	ASSERT(found != mMap.end());
	ASSERT(found->second == inDatum);
	mMap.erase(found);
}

void Map::DoInsert(ConstKeyType &inKey, Datum *inDatum)
{
	mMap.insert(DatumMap::value_type(inKey, inDatum));
}


//=========================================================================
//  List::InsertChange
//=========================================================================

class List::InsertChange : public Change {
	List *mCollection;
	Datum *mNewDatum;
	ConstKeyType mKey;

public:
	InsertChange(List *inCollection,
				 ConstKeyType &inKey,
				 Datum *inValue);

protected:
	virtual void DoApply();
	virtual void DoRevert();
	virtual void DoFreeApplyResources();
	virtual void DoFreeRevertResources();
};

List::InsertChange::InsertChange(List *inCollection,
								 ConstKeyType &inKey,
								 Datum *inValue)
	: mCollection(inCollection), mNewDatum(inValue), mKey(inKey)
{
}

void List::InsertChange::DoApply()
{
	mCollection->DoInsert(mKey, mNewDatum);
}

void List::InsertChange::DoRevert()
{
	mCollection->DoRemoveKnown(mKey, mNewDatum);
}

void List::InsertChange::DoFreeApplyResources()
{
	delete mNewDatum;
	mNewDatum = NULL;	
}

void List::InsertChange::DoFreeRevertResources()
{
}

void List::PerformInsert(ConstKeyType &inKey, Datum *inValue)
{
	RegisterChildObjectWithModel(inValue);
	ApplyChange(new InsertChange(this, inKey, inValue));
}


//=========================================================================
//  List Methods
//=========================================================================

void List::Write(xml_node inParent)
{
	xml_node node = inParent.new_child("list");
	DatumVector::iterator i = mVector.begin();
	for (; i != mVector.end(); ++i)
		(*i)->Write(node);
}

void List::Fill(xml_node inNode)
{
	xml_node::iterator i = inNode.begin();
	for (; i != inNode.end(); ++i)
	{
		xml_node node = *i;
		Datum *value = CreateFromXML(node);
		Insert(mVector.size(), value);
		value->Fill(node);
	}
}

Datum *List::DoGet(ConstKeyType &inKey)
{
	CHECK(inKey < mVector.size(), "No such key in List::DoGet");
	return mVector[inKey];
}

Datum *List::DoFind(ConstKeyType &inKey)
{
	if (inKey < mVector.size())
		return mVector[inKey];
	else
		return NULL;
}

void List::DoRemoveKnown(ConstKeyType &inKey, Datum *inDatum)
{
	// This runs in O(N) time, which isn't great.
	ASSERT(inKey < mVector.size());
	ASSERT(inDatum != NULL);
	ASSERT(mVector[inKey] == inDatum);
	mVector.erase(mVector.begin() + inKey);
}

void List::DoInsert(ConstKeyType &inKey, Datum *inDatum)
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
void model::Move(C1 *inDest, typename C1::ConstKeyType &inDestKey,
					 C2 *inSrc, typename C2::ConstKeyType &inSrcKey)
{
	inDest->GetModel()->
      ApplyChange(new MoveChange<typename C1::KeyType,
				                 typename C2::KeyType>(inDest, inDestKey,
													   inSrc, inSrcKey));
}

template MoveChange<size_t,size_t>;
template MoveChange<std::string,size_t>;
template MoveChange<size_t,std::string>;
template MoveChange<std::string,std::string>;

template void model::Move(List*, List::ConstKeyType&,
						  List*, List::ConstKeyType&);

template void model::Move(Map*, Map::ConstKeyType&,
						  Map*, Map::ConstKeyType&);
#endif // 0


//=========================================================================
//  Model Methods
//=========================================================================

void Model::Initialize()
{
	mChangePosition = mChanges.begin();
	mRoot = new Map();
	mRoot->RegisterWithModel(this);
}

Model::Model(const ModelFormat &inFormat)
	: mFormat(inFormat), mRoot(NULL)
{
	Initialize();
}

Model::Model(const ModelFormat &inCurrentFormat,
			 ModelFormat::Version inEarliestFormat,
			 const std::string &inPath)
	: mFormat(inCurrentFormat), mRoot(NULL)
{
	typedef ModelFormat::Version Version;

	Initialize();

	// Open the XML file.
	xmlDocPtr doc = xmlParseFile(inPath.c_str());
	try
	{
		CHECK(doc, "Failed to load XML file");

		// Get the root element.
		xmlNodePtr root_node = xmlDocGetRootElement(doc);
		CHECK(root_node, "No document root in XML file");
		xml_node root(root_node);

		// Check the format and version.
		ModelFormat file_format
			(root.name(),
			 lexical_cast<Version>(root.attribute("version")),
			 lexical_cast<Version>(root.attribute("backto")));
		CHECK(file_format.GetName() == mFormat.GetName(),
			  "XML file contains the wrong type of data");
		CHECK(mFormat.GetVersion() >= file_format.GetCompatibleBackTo(),
			  "XML file is in a newer, unsupported format");
		CHECK(file_format.GetVersion() >= inEarliestFormat,
			  "XML file is in a older, unsupported format");
		mFormat = file_format;

		// Get our top-level map and fill it out.
		xml_node map_node = xml_node(root).only_child();
		XML_CHECK_NAME(map_node, "map");
		mRoot->Fill(map_node);
	}
	catch (...)
	{
		if (doc)
			xmlFreeDoc(doc);
		throw;
	}

	xmlFreeDoc(doc);
	ClearUndoList();
}

Model::~Model()
{
	ClearRedoList();
	ClearUndoList();
}

bool Model::CanUndo()
{
	return mChangePosition != mChanges.begin();
}

void Model::Undo()
{
	ASSERT(CanUndo());
	(*--mChangePosition)->Revert();
}

void Model::ClearUndoList()
{
	ChangeList::iterator i = mChanges.begin();
	for (; i != mChangePosition; i = mChanges.erase(i))
	{
		(*i)->FreeResources();
		delete *i;
	}
	ASSERT(!CanUndo());
}

bool Model::CanRedo()
{
	return mChangePosition != mChanges.end();
}

void Model::Redo()
{
	ASSERT(CanRedo());
	(*mChangePosition++)->Apply();
}

void Model::ClearRedoList()
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

void Model::ApplyChange(Change *inChange)
{
	ClearRedoList();
	inChange->Apply();
	mChangePosition = mChanges.insert(mChangePosition, inChange);
	mChangePosition++;
}

void Model::SaveAs(const std::string &inFile)
{
	// Create a tree.
	xmlDocPtr doc = xmlNewDoc(xml_node::to_utf8("1.0"));
	doc->children =
		xmlNewDocNode(doc, NULL, xml_node::to_utf8(mFormat.GetName().c_str()),
					  NULL);
	xml_node root(doc->children);
	std::string vers(lexical_cast<std::string>(mFormat.GetVersion()));
	std::string back(lexical_cast<std::string>(mFormat.GetCompatibleBackTo()));
	root.set_attribute("version", vers);
	root.set_attribute("backto", back);

	// Add nodes as appropriate.
	mRoot->Write(root);

	// Serialize it to a file.
	int result = xmlSaveFormatFile(inFile.c_str(), doc, 1);
	xmlFreeDoc(doc);
	CHECK(result != -1, "Failed to save XML file");
}
