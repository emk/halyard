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
	TEST_EXCEPTION(root->GetValue<StringDatum>("test string"), TException);

	/*
	{
		Transaction t(store);
		root->SetValue<IntegerDatum>("test int", 20);
		TEST(root->GetValue<IntegerDatum>("test int") == 20);
	}
	TEST(root->GetValue<IntegerDatum>("test int") == 20);
	TEST(store.CanUndo() == true);
	TEST(store.CanRedo() == false);

	store.Undo();
	TEST(root->GetValue<IntegerDatum>("test int") == 10);
	TEST(store.CanUndo() == false);
	TEST(store.CanRedo() == true);
	*/
}
