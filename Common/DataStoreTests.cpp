// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "ImlUnit.h"
#include "TCommon.h"
#include "DataStore.h"

using namespace DataStore;

extern void test_DataStore (void);


//=========================================================================
//	DataStore Tests
//=========================================================================

void test_DataStore (void)
{
	Store store;
	TEST(store.CanUndo() == false);
	TEST(store.CanRedo() == false);

	MapDatum *root = store.GetRoot();
	TEST(root);

	// Test the basic value types.
	root->SetValue<IntegerDatum>("test int", 10);
	TEST(root->GetValue<IntegerDatum>("test int") == 10);
	root->SetValue<StringDatum>("test string", "foo");
	TEST(root->GetValue<StringDatum>("test string") == "foo");

	// Test the error-checking code.
	TEST_EXCEPTION(root->GetValue<IntegerDatum>("nosuch"), TException);

	// Make sure that Undo is enabled.
	TEST(store.CanUndo() == true);
	TEST(store.CanRedo() == false);

	// Test Undo by removing the "test string" key. 
	store.Undo();
	TEST(root->GetValue<IntegerDatum>("test int") == 10);
	TEST_EXCEPTION(root->GetValue<StringDatum>("test string"), TException);

	// Make sure that *both* Undo and Redo are enabled.
	TEST(store.CanUndo() == true);
	TEST(store.CanRedo() == true);

	// Test Undo by removing the "test int" key. 
	store.Undo();
	TEST_EXCEPTION(root->GetValue<IntegerDatum>("test int"), TException);
	TEST_EXCEPTION(root->GetValue<StringDatum>("test string"), TException);

	// Make sure that only Redo is enabled.
	TEST(store.CanUndo() == false);
	TEST(store.CanRedo() == true);

	// Test Redo.
	store.Redo();
	TEST(root->GetValue<IntegerDatum>("test int") == 10);
	TEST_EXCEPTION(root->GetValue<StringDatum>("test string"), TException);
	TEST(store.CanUndo() == true);
	TEST(store.CanRedo() == true);
	store.Redo();
	TEST(root->GetValue<IntegerDatum>("test int") == 10);
	TEST(root->GetValue<StringDatum>("test string") == "foo");
	TEST(store.CanUndo() == true);
	TEST(store.CanRedo() == false);
	
	// Test overwriting of Redo data.
	store.Undo();
	TEST_EXCEPTION(root->GetValue<StringDatum>("test string"), TException);
	root->SetValue<StringDatum>("test string", "bar");
	TEST(store.CanUndo() == true);
	TEST(store.CanRedo() == false);
	TEST(root->GetValue<StringDatum>("test string") == "bar");
	
	// Test clearing of Undo data.
	store.Undo();
	TEST(store.CanUndo() == true);
	TEST(store.CanRedo() == true);
	store.ClearUndoList();
	TEST(store.CanUndo() == false);
	TEST(store.CanRedo() == true);
	store.Redo();
	TEST(store.CanUndo() == true);
	TEST(store.CanRedo() == false);
	TEST(root->GetValue<StringDatum>("test string") == "bar");

	//---------------------------------------------------------------------
	// OK, we pretty much believe that Undo/Redo are correct.  Now
	// we need to test ALL the subclasses of Change.

	// Insert a new map datum to work with.
	root->Set<MapDatum>("map", new MapDatum());
	MapDatum *map = root->Get<MapDatum>("map");

	// Test MapDatum::SetChange.
	map->SetValue<StringDatum>("SetChange", "new key");
	TEST(map->GetValue<StringDatum>("SetChange") == "new key");
	store.Undo();
	TEST_EXCEPTION(map->GetValue<StringDatum>("SetChange"), TException);
	store.Redo();
	TEST(map->GetValue<StringDatum>("SetChange") == "new key");
	map->SetValue<StringDatum>("SetChange", "change value");
	TEST(map->GetValue<StringDatum>("SetChange") == "change value");
	store.Undo();
	TEST(map->GetValue<StringDatum>("SetChange") == "new key");
	store.Redo();
	TEST(map->GetValue<StringDatum>("SetChange") == "change value");
	
	/*
	{
		Transaction t(store);
		root->SetValue<IntegerDatum>("test int", 20);
		TEST(root->GetValue<IntegerDatum>("test int") == 20);
	}
	*/
}
