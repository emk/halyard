// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#include "CommonHeaders.h"
#include "TestCase.h"
#include "TStateDB.h"

USING_NAMESPACE_FIVEL

REGISTER_TEST_CASE_FILE(TStateDB);
	
class TStateDB;

TStateDB FIVEL_NS gStateDB;


//=========================================================================
//  TStateListener methods
//=========================================================================

TStateListener::~TStateListener() {
	if (mDB)
		mDB->UnregisterListener(this, mKeys);
}

void TStateListener::RecordKey(TStateDB *db, const std::string &inKey) {
	ASSERT(mDB == NULL || mDB == db);
	mDB = db;
	if (std::find(mKeys.begin(), mKeys.end(), inKey) == mKeys.end())
		mKeys.push_back(inKey);
}

 
//=========================================================================
//  TStateDB::Datum methods
//========================================================================= 

void TStateDB::Datum::EnsureListenerRegistered(TStateListener *inListener) {
	ListenerList::iterator iter =
		std::find(mListeners.begin(), mListeners.end(), inListener);
	if (iter == mListeners.end())
		mListeners.push_back(inListener);
}

void TStateDB::Datum::UnregisterListener(TStateListener *inListener) {
	ListenerList::iterator iter =
		std::find(mListeners.begin(), mListeners.end(), inListener);
	ASSERT(iter != mListeners.end());
	mListeners.erase(iter);
}

void TStateDB::Datum::NotifyListeners() {
    // XXX - Iterate through a copy of this list in case
    // TStateListeners are managing to unregister themselves.
	// Let's not mention what that does to NotifyStateChanged.
	ListenerList copy = mListeners;
	ListenerList::iterator iter = copy.begin(); 
	for (; iter != copy.end(); ++iter)
		(*iter)->NotifyStateChanged();
}

void TStateDB::Datum::MaybeSetVal(TStateDB *inDB, TValue inValue) {
	if (mValue != inValue) {
		if (inDB->mNotifyCount >= MAX_RECURSION)
			THROW("TStateDB does not allow infinitely recursive changes.");

		mValue = inValue;

		++inDB->mNotifyCount;
		try {
			NotifyListeners();
		} catch(...) {
			--inDB->mNotifyCount;
			throw;
		}
		--inDB->mNotifyCount;
	}
}


//=========================================================================
//  TStateDB methods
//========================================================================= 

void TStateDB::CheckForLegalKey(const std::string &inKey) {
	if (inKey.size() == 0 ||
		inKey[0] != '/' ||
		inKey[inKey.size() - 1] == '/' ||
		inKey.find("//") != std::string::npos)
	{
		THROW("Malformed state database key: " + inKey);
	}
}

void TStateDB::UnregisterListener(TStateListener *inListener, 
								  std::vector<std::string> inKeyList) 
{
	std::vector<std::string>::iterator iter = inKeyList.begin();
	for (; iter != inKeyList.end(); ++iter) {
		DatumMap::iterator found = mDB.find(*iter);
		ASSERT(found != mDB.end());
		found->second.UnregisterListener(inListener);
	}
}

void TStateDB::Set(const std::string &inKey, TValue inValue) {
	CheckForLegalKey(inKey);
	DatumMap::iterator found = mDB.find(inKey);
	if (found != mDB.end())
		found->second.MaybeSetVal(this, inValue);
	else
		mDB.insert(DatumMap::value_type(inKey, Datum(inValue)));
}

TValue TStateDB::Get(TStateListener *inListener, const std::string &inKey) {
	DatumMap::iterator found = mDB.find(inKey);
	if (found != mDB.end()) {
		found->second.EnsureListenerRegistered(inListener);
		inListener->RecordKey(this, inKey);
		return found->second.mValue;
	} else {
		THROW("The key '" + inKey + "\' is not in the state database.");
	}
}


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

class DummyListener : public TStateListener {
public:
	virtual void NotifyStateChanged() {} 		
};

class TestListener : public TStateListener 
{
	int mUpdateCount;
	
public:	
	TestListener() : mUpdateCount(0) {}
	
	virtual void NotifyStateChanged(){++mUpdateCount;}
	int GetUpdateCount() { return mUpdateCount; }	
};

class RecursiveListener : public TStateListener {

	TStateDB *mStateDb;
	std::string mTargetKey;
	int mTargetVal;

public:
	RecursiveListener(TStateDB *inDB, std::string inTargetKey) 
		: mStateDb(inDB), mTargetKey(inTargetKey), mTargetVal(0) {}

	virtual void NotifyStateChanged() {
		mStateDb->Set(mTargetKey, mTargetVal++);
	}
};

void CHECK_DB_EQ(TStateDB &db, const std::string &inKey, TValue inValue) {
	CHECK_EQ(db.Get(&DummyListener(), inKey), inValue);
}

BEGIN_TEST_CASE(TestTStateDB, TestCase) {
	TStateDB db;

	// Unattached listeners shouldn't crash.
	{
		DummyListener dummy;
	}

	// Non-existant keys raise errors.
	CHECK_THROWN(std::exception, db.Get(&DummyListener(), "/nosuch"));

	// The database works like a map.
	db.Set("/foo", "bar");
	db.Set("/dog", "fido");
	CHECK_DB_EQ(db, "/foo", "bar");
	CHECK_DB_EQ(db, "/dog", "fido");
	db.Set("/dog", "bruno");
	CHECK_DB_EQ(db, "/dog", "bruno");

	// Keys look like Unix pathnames or registry entries.
	CHECK_THROWN(std::exception, db.Set("badkey", "foo"));
	CHECK_THROWN(std::exception, db.Set("/badkey/", "foo"));
	CHECK_THROWN(std::exception, db.Set("/", "foo"));
	db.Set("/good/key", "foo");
	CHECK_DB_EQ(db, "/good/key", "foo");
	CHECK_THROWN(std::exception, db.Set("/bad//key", "foo"));

	// Test TStateListener.
	{
		// listener1 will only live as long as this block.
		TestListener listener1;
		TestListener listener2;

		db.Set("/listener/1", "foo");
		CHECK_EQ(db.Get(&listener1, "/listener/1"), "foo");
		CHECK_EQ(listener1.GetUpdateCount(), 0);
		db.Set("/listener/1", "foo");
		CHECK_EQ(listener1.GetUpdateCount(), 0);
		db.Set("/listener/1", "bar");
		CHECK_EQ(listener1.GetUpdateCount(), 1);
		db.Set("/listener/ignored", "moby");
		CHECK_EQ(listener1.GetUpdateCount(), 1);

		// Adding a new listener to the same event should should
		// update both listeners correctly when Set is called.
		db.Get(&listener2, "/listener/1");
		db.Set("/listener/1", "foo");
		CHECK_EQ(listener1.GetUpdateCount(), 2);
		CHECK_EQ(listener2.GetUpdateCount(), 1);

		// Updates on listener1 from another event should 
		// not affect listener2.
		db.Set("/listener/2", 23.0);
		db.Get(&listener1, "/listener/2");
		db.Set("/listener/2", TSymbol("dog"));
		CHECK_EQ(listener1.GetUpdateCount(), 3);
		CHECK_EQ(listener2.GetUpdateCount(), 1);

		// Make sure listeners can't get registered twice.
		db.Get(&listener1, "/listener/2");
		CHECK_EQ(listener1.GetUpdateCount(), 3);
	}
	// Hmmm.  Our listener is gone; what happens if we change this
	// key's value now?
	db.Set("/listener/1", "foo");
	CHECK_DB_EQ(db, "/listener/1", "foo");

    // Make sure infinite event handling loops don't occur
	{
		RecursiveListener listener1 = 
			RecursiveListener(&db, "/listener/2");
		RecursiveListener listener2 = 
			RecursiveListener(&db, "/listener/1");
		
		db.Set("/listener/1", "foo");
		db.Set("/listener/2", 23.0);
		db.Get(&listener1, "/listener/1");
		db.Get(&listener2, "/listener/2");

		CHECK_THROWN(std::exception,
					 db.Set("/listener/2", "bar"));
	}	

} END_TEST_CASE(TestTStateDB);

#endif // BUILD_TEST_CASES
