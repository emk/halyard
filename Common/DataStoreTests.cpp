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

	root->SetValue<IntegerDatum>("test int", 10);
	TEST(root->GetValue<IntegerDatum>("test int") == 10);
	root->SetValue<StringDatum>("test string", "foo");
	TEST(root->GetValue<StringDatum>("test string") == "foo");
}
