// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "ImlUnit.h"
#include "TCommon.h"
#include "Model.h"

using namespace model;

extern void test_Model (void);


//=========================================================================
//	Sample Subclasses of Object
//=========================================================================

class TestData : public Object {
public:
	DECLARE_MODEL_CLASS(TestData);
};

IMPLEMENT_MODEL_CLASS(TestData);

class Card : public Object {
public:
	DECLARE_MODEL_CLASS(Card);
};

IMPLEMENT_MODEL_CLASS(Card);

BEGIN_MODEL_CLASSES()
	REGISTER_MODEL_CLASS(TestData)
	REGISTER_MODEL_CLASS(Card)
END_MODEL_CLASSES()


//=========================================================================
//	Model Tests
//=========================================================================

void test_Model (void)
{
	ModelFormat format("TestData", 0, 0);
	Model model(format);
	TEST(model.CanUndo() == false);
	TEST(model.CanRedo() == false);

	TestData *root = cast<TestData>(model.GetRoot());
	TEST(root);

	// Test the basic value types.
	root->SetInteger("testInt", 10);
	TEST(root->GetInteger("testInt") == 10);
	root->SetString("testString", "foo");
	TEST(root->GetString("testString") == "foo");

	// Test the error-checking code.
	TEST_EXCEPTION(root->GetInteger("nosuch"), TException);

	// Make sure that Undo is enabled.
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == false);

	// Test Undo by removing the "test string" key. 
	model.Undo();
	TEST(root->GetInteger("testInt") == 10);
	TEST_EXCEPTION(root->GetString("testString"), TException);

	// Make sure that *both* Undo and Redo are enabled.
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == true);

	// Test Undo by removing the "test int" key. 
	model.Undo();
	TEST_EXCEPTION(root->GetInteger("testInt"), TException);
	TEST_EXCEPTION(root->GetString("testString"), TException);

	// Make sure that only Redo is enabled.
	TEST(model.CanUndo() == false);
	TEST(model.CanRedo() == true);

	// Test Redo.
	model.Redo();
	TEST(root->GetInteger("testInt") == 10);
	TEST_EXCEPTION(root->GetString("testString"), TException);
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == true);
	model.Redo();
	TEST(root->GetInteger("testInt") == 10);
	TEST(root->GetString("testString") == "foo");
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == false);
	
	// Test overwriting of Redo data.
	model.Undo();
	TEST_EXCEPTION(root->GetString("testString"), TException);
	root->SetString("testString", "bar");
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == false);
	TEST(root->GetString("testString") == "bar");
	
	// Test clearing of Undo data.
	model.Undo();
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == true);
	model.ClearUndoList();
	TEST(model.CanUndo() == false);
	TEST(model.CanRedo() == true);
	model.Redo();
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == false);
	TEST(root->GetString("testString") == "bar");

	//---------------------------------------------------------------------
	// OK, we pretty much believe that Undo/Redo are correct.  Now
	// we need to test ALL the subclasses of Change.

	// Insert a new map datum to work with.
	root->Set("testMap", new Map());
	Map *map = cast<Map>(root->Get("testMap"));

	// Test CollectionDatum::SetChange on Map.
	map->SetString("SetChange", "new key");
	TEST(map->GetString("SetChange") == "new key");
	model.Undo();
	TEST_EXCEPTION(map->GetString("SetChange"), TException);
	model.Redo();
	TEST(map->GetString("SetChange") == "new key");
	map->SetString("SetChange", "change value");
	TEST(map->GetString("SetChange") == "change value");
	model.Undo();
	TEST(map->GetString("SetChange") == "new key");
	model.Redo();
	TEST(map->GetString("SetChange") == "change value");

	// Test CollectionDatum::DeleteChange on Map.
	TEST_EXCEPTION(map->Delete("nosuch"), TException);
	map->SetString("DeleteChange", "foo");
	TEST(map->GetString("DeleteChange") == "foo");
	map->Delete("DeleteChange");
	TEST_EXCEPTION(map->Delete("DeleteChange"), TException);
	model.Undo();
	TEST(map->GetString("DeleteChange") == "foo");
	model.Redo();
	TEST_EXCEPTION(map->Delete("DeleteChange"), TException);

	// Create a new List.
	root->Set("testList", new List());
	List *list = cast<List>(root->Get("testList"));

	// Insert two elements.
	list->InsertString(0, "item 0");
	TEST(list->GetString(0) == "item 0");
	list->InsertString(1, "item 1");
	TEST(list->GetString(1) == "item 1");

	// Test Map::SetChange and List::InsertChange on List.
	model.Undo();
	TEST(list->GetString(0) == "item 0");
	TEST_EXCEPTION(list->GetString(1), TException);
	model.Redo();
	TEST(list->GetString(0) == "item 0");
	TEST(list->GetString(1) == "item 1");
	list->InsertString(1, "item 0.5");
	TEST(list->GetString(0) == "item 0");
	TEST(list->GetString(1) == "item 0.5");
	TEST(list->GetString(2) == "item 1");
	model.Undo();
	TEST(list->GetString(0) == "item 0");
	TEST(list->GetString(1) == "item 1");
	model.Redo();
	TEST(list->GetString(0) == "item 0");
	TEST(list->GetString(1) == "item 0.5");
	TEST(list->GetString(2) == "item 1");
	list->SetString(1, "item 1/2");
	TEST(list->GetString(0) == "item 0");
	TEST(list->GetString(1) == "item 1/2");
	TEST(list->GetString(2) == "item 1");
	model.Undo();
	TEST(list->GetString(0) == "item 0");
	TEST(list->GetString(1) == "item 0.5");
	TEST(list->GetString(2) == "item 1");
	model.Undo();
	TEST(list->GetString(0) == "item 0");
	TEST(list->GetString(1) == "item 1");

	//---------------------------------------------------------------------
	// Move is an especially tricky case.

#if 0
	// Create some collections to move between.
	root->Set("moveMap1", new Map());	
	root->Set("moveMap2", new Map());
	root->Set("moveList1", new List());	
	root->Set("moveList2", new List());
	Map *mmap1 = cast<Map>(root->Get("moveMap1"));
	Map *mmap2 = cast<Map>(root->Get("moveMap2"));
	//List *mlist1 = cast<List>(root->Get("moveList1"));
	//List *mlist2 = cast<List>(root->Get("moveList2"));
	
	// Add some elements.
	mmap1->SetString("foo", "foo value");

	// Test various sorts of moves.
	TEST(mmap1->GetString("foo") == "foo value");
	TEST_EXCEPTION(mmap2->GetString("new foo"), TException);
	Move(mmap2, "new foo", mmap1, "foo");
	TEST_EXCEPTION(mmap1->GetString("foo"), TException);
	TEST(mmap2->GetString("new foo") == "foo value");
	model.Undo();
	TEST(mmap1->GetString("foo") == "foo value");
	TEST_EXCEPTION(mmap2->GetString("new foo"), TException);
	model.Redo();
	TEST_EXCEPTION(mmap1->GetString("foo"), TException);
	TEST(mmap2->GetString("new foo") == "foo value");
#endif // 0

	/*
	{
		Transaction t(model);
		root->SetInteger("testInt", 20);
		TEST(root->GetInteger("testInt") == 20);
	}
	*/

	//---------------------------------------------------------------------
	// Test Objects

	Card *card = root->Set("sampleCard", new Card());
	TEST(cast<Card>(root->Get("sampleCard")) == card);
	card->SetString("name", "CardName");

	//---------------------------------------------------------------------
	// Test Serialization

	model.SaveAs("model.xml");

	Model model2(format, 0, "model.xml");
	TEST(model2.CanUndo() == false);
	TEST(model2.CanRedo() == false);

	model2.SaveAs("model2.xml");

	TestData *root2 = cast<TestData>(model2.GetRoot());
	TEST(root2->GetInteger("testInt") == 10);
	TEST(root2->GetString("testString") == "bar");
}
