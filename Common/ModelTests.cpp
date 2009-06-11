// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#include "ImlUnit.h"
#include "Model.h"
#include "ModelView.h"

using namespace Halyard;
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

// There's no need to register these since we use them in this file,
// and if we do register them, it will conflict with the ones we use for
// the real program (unless somebody fixes BEGIN_MODEL_CLASSES).
//BEGIN_MODEL_CLASSES()
//	REGISTER_MODEL_CLASS(TestData)
//	REGISTER_MODEL_CLASS(Card)
//END_MODEL_CLASSES()


//=========================================================================
//	Sample View Class
//=========================================================================

class CardView : public View {
	int mChangeCount;
	int mDeleteCount;
	bool mCurrentlyDeleted;
	std::string mCardName;
	bool mHaveListItem;
	std::string mListItem;

public:
	CardView();

	virtual void ObjectDeleted();
	virtual void ObjectChanged();

	void CheckCounts(int inChanges, int inDeletes);

	bool IsCurrentlyDeleted() { return mCurrentlyDeleted; }
	std::string GetName() { return mCardName; }
	bool HaveListItem() { return mHaveListItem; }
	std::string GetListItem() { return mListItem; }
};

CardView::CardView()
{
	mChangeCount = 0;
	mDeleteCount = 0;
}

void CardView::CheckCounts(int inChanges, int inDeletes)
{
	TEST(mChangeCount == inChanges);
	TEST(mDeleteCount == inDeletes);
}

void CardView::ObjectDeleted()
{
	mDeleteCount++;
	mCurrentlyDeleted = true;
}

void CardView::ObjectChanged()
{
	mChangeCount++;
	mCurrentlyDeleted = false;
	mCardName = GetObject()->GetString("name");
	mHaveListItem = false;
	if (GetObject()->HaveKey("list"))
	{
		List *list = cast<List>(GetObject()->Get("list"));
		if (list->HaveKey(0))
		{
			mHaveListItem = true;
			mListItem = list->GetString(0);
		}
	}
}


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
	TEST(card->GetString("name") == "CardName");

	//---------------------------------------------------------------------
	// Test Views

	{
		// Test direct changes to Object.
		CardView view1;
		view1.SetObject(card);
		TEST(view1.GetName() == "CardName");
		TEST(view1.GetObject() == card);
		card->SetString("name", "CardName2");
		TEST(view1.GetName() == "CardName2");
		model.Undo();
		TEST(view1.GetName() == "CardName");
		model.Redo();
		TEST(view1.GetName() == "CardName2");
	}

	{
		// Test changes to non-Objects contained within our Object.
		// These changes should propagate up to the appropriate object.
		CardView view2;
		view2.SetObject(card);
		TEST(view2.GetName() == "CardName2");
		TEST(view2.HaveListItem() == false);
		List *small_list = card->Set("list", new List());
		TEST(view2.HaveListItem() == false);
		small_list->InsertString(0, "foo");
		TEST(view2.HaveListItem() == true);
		TEST(view2.GetListItem() == "foo");
		model.Undo();
		TEST(view2.HaveListItem() == false);
		model.Redo();
		TEST(view2.HaveListItem() == true);
		TEST(view2.GetListItem() == "foo");		
	}
	
	{
		// Test ObjectDeleted notifications.
		CardView view3;
		view3.SetObject(card);
		Card *subcard = card->Set("subcard", new Card());
		subcard->SetString("name", "subcard");
		CardView subview;
		subview.SetObject(subcard);
		Card *subsubcard = subcard->Set("subsubcard", new Card());
		subsubcard->SetString("name", "subsubcard");
		CardView subsubview;
		subsubview.SetObject(subsubcard);
		view3.CheckCounts(2, 0);
		subview.CheckCounts(2, 0);
		subsubview.CheckCounts(1, 0);
		TEST(view3.ObjectIsLive() && !view3.IsCurrentlyDeleted());
		TEST(subview.ObjectIsLive() && !subview.IsCurrentlyDeleted());
		TEST(subsubview.ObjectIsLive() && !subsubview.IsCurrentlyDeleted());
		card->Delete("subcard");
		view3.CheckCounts(3, 0);
		subview.CheckCounts(2, 1);
		subsubview.CheckCounts(1, 1);
		TEST(view3.ObjectIsLive() && !view3.IsCurrentlyDeleted());
		TEST(!subview.ObjectIsLive() && subview.IsCurrentlyDeleted());
		TEST(!subsubview.ObjectIsLive() && subsubview.IsCurrentlyDeleted());
		model.Undo();
		view3.CheckCounts(4, 0);
		subview.CheckCounts(3, 1);
		subsubview.CheckCounts(2, 1);
		TEST(view3.ObjectIsLive() && !view3.IsCurrentlyDeleted());
		TEST(subview.ObjectIsLive() && !subview.IsCurrentlyDeleted());
		TEST(subsubview.ObjectIsLive() && !subsubview.IsCurrentlyDeleted());
		subcard->Delete("subsubcard");
		view3.CheckCounts(4, 0);
		subview.CheckCounts(4, 1);
		subsubview.CheckCounts(2, 2);
		TEST(view3.ObjectIsLive() && !view3.IsCurrentlyDeleted());
		TEST(subview.ObjectIsLive() && !subview.IsCurrentlyDeleted());
		TEST(!subsubview.ObjectIsLive() && subsubview.IsCurrentlyDeleted());
	}

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
