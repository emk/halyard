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
	root->SetValue<Integer>("test int", 10);
	TEST(root->GetValue<Integer>("test int") == 10);
	root->SetValue<String>("test string", "foo");
	TEST(root->GetValue<String>("test string") == "foo");

	// Test the error-checking code.
	TEST_EXCEPTION(root->GetValue<Integer>("nosuch"), TException);

	// Make sure that Undo is enabled.
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == false);

	// Test Undo by removing the "test string" key. 
	model.Undo();
	TEST(root->GetValue<Integer>("test int") == 10);
	TEST_EXCEPTION(root->GetValue<String>("test string"), TException);

	// Make sure that *both* Undo and Redo are enabled.
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == true);

	// Test Undo by removing the "test int" key. 
	model.Undo();
	TEST_EXCEPTION(root->GetValue<Integer>("test int"), TException);
	TEST_EXCEPTION(root->GetValue<String>("test string"), TException);

	// Make sure that only Redo is enabled.
	TEST(model.CanUndo() == false);
	TEST(model.CanRedo() == true);

	// Test Redo.
	model.Redo();
	TEST(root->GetValue<Integer>("test int") == 10);
	TEST_EXCEPTION(root->GetValue<String>("test string"), TException);
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == true);
	model.Redo();
	TEST(root->GetValue<Integer>("test int") == 10);
	TEST(root->GetValue<String>("test string") == "foo");
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == false);
	
	// Test overwriting of Redo data.
	model.Undo();
	TEST_EXCEPTION(root->GetValue<String>("test string"), TException);
	root->SetValue<String>("test string", "bar");
	TEST(model.CanUndo() == true);
	TEST(model.CanRedo() == false);
	TEST(root->GetValue<String>("test string") == "bar");
	
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
	TEST(root->GetValue<String>("test string") == "bar");

	//---------------------------------------------------------------------
	// OK, we pretty much believe that Undo/Redo are correct.  Now
	// we need to test ALL the subclasses of Change.

	// Insert a new map datum to work with.
	root->Set<Map>("map", new Map());
	Map *map = root->Get<Map>("map");

	// Test CollectionDatum::SetChange on Map.
	map->SetValue<String>("SetChange", "new key");
	TEST(map->GetValue<String>("SetChange") == "new key");
	model.Undo();
	TEST_EXCEPTION(map->GetValue<String>("SetChange"), TException);
	model.Redo();
	TEST(map->GetValue<String>("SetChange") == "new key");
	map->SetValue<String>("SetChange", "change value");
	TEST(map->GetValue<String>("SetChange") == "change value");
	model.Undo();
	TEST(map->GetValue<String>("SetChange") == "new key");
	model.Redo();
	TEST(map->GetValue<String>("SetChange") == "change value");

	// Test CollectionDatum::DeleteChange on Map.
	TEST_EXCEPTION(map->Delete("nosuch"), TException);
	map->SetValue<String>("DeleteChange", "foo");
	TEST(map->GetValue<String>("DeleteChange") == "foo");
	map->Delete("DeleteChange");
	TEST_EXCEPTION(map->Delete("DeleteChange"), TException);
	model.Undo();
	TEST(map->GetValue<String>("DeleteChange") == "foo");
	model.Redo();
	TEST_EXCEPTION(map->Delete("DeleteChange"), TException);

	// Create a new List.
	root->Set<List>("list", new List());
	List *list = root->Get<List>("list");

	// Insert two elements.
	list->InsertValue<String>(0, "item 0");
	TEST(list->GetValue<String>(0) == "item 0");
	list->InsertValue<String>(1, "item 1");
	TEST(list->GetValue<String>(1) == "item 1");

	// Test Map::SetChange and List::InsertChange on List.
	model.Undo();
	TEST(list->GetValue<String>(0) == "item 0");
	TEST_EXCEPTION(list->GetValue<String>(1), TException);
	model.Redo();
	TEST(list->GetValue<String>(0) == "item 0");
	TEST(list->GetValue<String>(1) == "item 1");
	list->InsertValue<String>(1, "item 0.5");
	TEST(list->GetValue<String>(0) == "item 0");
	TEST(list->GetValue<String>(1) == "item 0.5");
	TEST(list->GetValue<String>(2) == "item 1");
	model.Undo();
	TEST(list->GetValue<String>(0) == "item 0");
	TEST(list->GetValue<String>(1) == "item 1");
	model.Redo();
	TEST(list->GetValue<String>(0) == "item 0");
	TEST(list->GetValue<String>(1) == "item 0.5");
	TEST(list->GetValue<String>(2) == "item 1");
	list->SetValue<String>(1, "item 1/2");
	TEST(list->GetValue<String>(0) == "item 0");
	TEST(list->GetValue<String>(1) == "item 1/2");
	TEST(list->GetValue<String>(2) == "item 1");
	model.Undo();
	TEST(list->GetValue<String>(0) == "item 0");
	TEST(list->GetValue<String>(1) == "item 0.5");
	TEST(list->GetValue<String>(2) == "item 1");
	model.Undo();
	TEST(list->GetValue<String>(0) == "item 0");
	TEST(list->GetValue<String>(1) == "item 1");

	//---------------------------------------------------------------------
	// Move is an especially tricky case.

#if 0
	// Create some collections to move between.
	root->Set<Map>("move map 1", new Map());	
	root->Set<Map>("move map 2", new Map());
	root->Set<List>("move list 1", new List());	
	root->Set<List>("move list 2", new List());
	Map *mmap1 = root->Get<Map>("move map 1");
	Map *mmap2 = root->Get<Map>("move map 2");
	//List *mlist1 = root->Get<List>("move list 1");
	//List *mlist2 = root->Get<List>("move list 2");
	
	// Add some elements.
	mmap1->SetValue<String>("foo", "foo value");

	// Test various sorts of moves.
	TEST(mmap1->GetValue<String>("foo") == "foo value");
	TEST_EXCEPTION(mmap2->GetValue<String>("new foo"), TException);
	Move(mmap2, "new foo", mmap1, "foo");
	TEST_EXCEPTION(mmap1->GetValue<String>("foo"), TException);
	TEST(mmap2->GetValue<String>("new foo") == "foo value");
	model.Undo();
	TEST(mmap1->GetValue<String>("foo") == "foo value");
	TEST_EXCEPTION(mmap2->GetValue<String>("new foo"), TException);
	model.Redo();
	TEST_EXCEPTION(mmap1->GetValue<String>("foo"), TException);
	TEST(mmap2->GetValue<String>("new foo") == "foo value");
#endif // 0

	/*
	{
		Transaction t(model);
		root->SetValue<Integer>("test int", 20);
		TEST(root->GetValue<Integer>("test int") == 20);
	}
	*/

	//---------------------------------------------------------------------
	// Test Objects

	Card *card = root->Set<Card>("sample card", new Card());
	TEST(root->Get<Card>("sample card") == card);
	card->SetValue<String>("name", "CardName");


	//---------------------------------------------------------------------
	// Test Serialization

	model.SaveAs("model.xml");

	Model model2(format, 0, "model.xml");
	TEST(model2.CanUndo() == false);
	TEST(model2.CanRedo() == false);

	model2.SaveAs("model2.xml");
	TEST(root->GetValue<Integer>("test int") == 10);
	TEST(root->GetValue<String>("test string") == "bar");
}
