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
		DeleteValue(*i);
}

void DatumMap::DeleteValue(value_type &inValue)
{
	delete inValue.second;
}

DatumList::~DatumList()
{
	for (iterator i = begin(); i != end(); i++)
		DeleteValue(*i);	
}

void DatumList::DeleteValue(value_type &inValue)
{
	delete inValue;
}

//=========================================================================
//  MapDatum Methods
//=========================================================================

void MapDatum::Set(ConstKeyType &inKey, Datum *inValue)
{
	DatumMap::iterator found = mMap.find(inKey);
	if (found != mMap.end())
	{
		mMap.DeleteValue(*found);
		mMap.erase(found);
	}
	mMap.insert(DatumMap::value_type(inKey, inValue));
}

Datum *MapDatum::Get(ConstKeyType &inKey)
{
	DatumMap::iterator found = mMap.find(inKey);
	if (found == mMap.end())
		throw TException(__FILE__, __LINE__, "MapDatum::Get: Can't find key");
	return found->second;
}


//=========================================================================
//  DataStore Methods
//=========================================================================

Store::Store()
{
	mRoot = new MapDatum();
}
