// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "ImlUnit.h"
#include "TCommon.h"
#include "Model.h"

using namespace model;

extern void test_Model (void);


//=========================================================================
//	Sample Subclass of Object
//=========================================================================

class Card : public Object {
public:
	static Class class_info;
	static Object *create_object();
	Card() : Object(&class_info) {}

};

Class Card::class_info("Card", &Card::create_object);
Object *Card::create_object() { return new Card(); }


//=========================================================================
//	Model Tests
//=========================================================================

void test_Model (void)
{
	ModelFormat format("TestData", 0, 0);
	Model model(format);
	TEST(model.CanUndo() == false);
	TEST(model.CanRedo() == false);

	Map *root = model.GetRoot();
	TEST(root);

	// Test the basic value types.
	root->SetInteger("test int", 10);
	TEST(root->GetInteger("test int") == 10);
	root->SetString("test string", "foo");
	TEST(root->GetString("test string") == "foo");

	// Test the error-checking code.
	TEST_EXCEPTION(root->GetInteger("nosuch"), TException);

	// Make sure that Undo is enabled.
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == false);

	// Test Undo by removing the "test string" key. 
	model.Undo();
	TEST(root->GetInteger("test int") == 10);
	TEST_EXCEPTION(root->GetString("test string"), TException);

	// Make sure that *both* Undo and Redo are enabled.
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == true);

	// Test Undo by removing the "test int" key. 
	model.Undo();
	TEST_EXCEPTION(root->GetInteger("test int"), TException);
	TEST_EXCEPTION(root->GetString("test string"), TException);

	// Make sure that only Redo is enabled.
	TEST(model.CanUndo() == false);
	TEST(model.CanRedo() == true);

	// Test Redo.
	model.Redo();
	TEST(root->GetInteger("test int") == 10);
	TEST_EXCEPTION(root->GetString("test string"), TException);
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == true);
	model.Redo();
	TEST(root->GetInteger("test int") == 10);
	TEST(root->GetString("test string") == "foo");
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == false);
	
	// Test overwriting of Redo data.
	model.Undo();
	TEST_EXCEPTION(root->GetString("test string"), TException);
	root->SetString("test string", "bar");
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == false);
	TEST(root->GetString("test string") == "bar");
	
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
	TEST(root->GetString("test string") == "bar");

	//---------------------------------------------------------------------
	// OK, we pretty much believe that Undo/Redo are correct.  Now
	// we need to test ALL the subclasses of Change.

	// Insert a new map datum to work with.
	root->Set("map", new Map());
	Map *map = cast<Map>(root->Get("map"));

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
	root->Set("list", new List());
	List *list = cast<List>(root->Get("list"));

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
	root->Set("move map 1", new Map());	
	root->Set("move map 2", new Map());
	root->Set("move list 1", new List());	
	root->Set("move list 2", new List());
	Map *mmap1 = cast<Map>(root->Get("move map 1"));
	Map *mmap2 = cast<Map>(root->Get("move map 2"));
	//List *mlist1 = cast<List>(root->Get("move list 1"));
	//List *mlist2 = cast<List>(root->Get("move list 2"));
	
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
		root->SetInteger("test int", 20);
		TEST(root->GetInteger("test int") == 20);
	}
	*/

	//---------------------------------------------------------------------
	// Test Objects

	Card *card = root->Set("sample card", new Card());
	TEST(cast<Card>(root->Get("sample card")) == card);
	card->SetString("name", "CardName");

	//---------------------------------------------------------------------
	// Test Serialization

	model.SaveAs("model.xml");

	Model model2(format, 0, "model.xml");
	TEST(model2.CanUndo() == false);
	TEST(model2.CanRedo() == false);

	model2.SaveAs("model2.xml");
	TEST(root->GetInteger("test int") == 10);
	TEST(root->GetString("test string") == "bar");
}
