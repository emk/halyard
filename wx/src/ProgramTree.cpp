// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include <wx/treectrl.h>
#include <wx/laywin.h>
#include <wx/imaglist.h>

#include "TCommon.h"
#include "TInterpreter.h"
#include "TVectorDiff.h"

#include <map>
#include <string>
#include <memory>

#include "AppGlobals.h"
#include "Stage.h"
#include "ProgramTree.h"
#include "Model.h"
#include "doc/Document.h"
#include "doc/TamaleProgram.h"
#include "dlg/ProgramPropDlg.h"

//=========================================================================
//  ProgramTreeCtrl
//=========================================================================

class ProgramTreeCtrl : public wxTreeCtrl
{
	DECLARE_EVENT_TABLE()

public:
	ProgramTreeCtrl(wxWindow *inParent, int id);

	//////////
	// Any of these icons may be used by nodes in the ProgramTree.
	//
	enum {
		ICON_CARD,
		ICON_DOCUMENT,
		ICON_FOLDER_CLOSED,
		ICON_FOLDER_OPEN
	};

public:
	void SetIcon(wxTreeItemId id, int closed_icon, int open_icon);

private:
	void BuildIconList();

	void OnLeftDClick(wxMouseEvent& event);
	void OnRightDown(wxMouseEvent& event);
};	


//=========================================================================
//  ProgramTreeItemData
//=========================================================================
//  This class respresents a "smart" node in our tree.  Most node-specific
//  events will be passed to a subclass of ProgramTreeItemData by our
//  event handlers.

class ProgramTreeItemData : public wxTreeItemData
{
	ProgramTreeCtrl *mTreeCtrl;

public:
	ProgramTreeItemData(ProgramTreeCtrl *inTreeCtrl);
	ProgramTreeCtrl *GetTree() { return mTreeCtrl; }

	virtual void OnLeftDClick(wxMouseEvent& event) {}
	virtual void OnRightDown(wxMouseEvent& event) {}
};

ProgramTreeItemData::ProgramTreeItemData(ProgramTreeCtrl *inTreeCtrl)
	: mTreeCtrl(inTreeCtrl)
{
	ASSERT(mTreeCtrl != NULL);
}


//=========================================================================
//  ViewItemData
//=========================================================================

class ViewItemData : public ProgramTreeItemData, public model::View
{
public:
	ViewItemData(ProgramTreeCtrl *inTreeCtrl)
		: ProgramTreeItemData(inTreeCtrl) {}	
};


//=========================================================================
//  CardItemData
//=========================================================================

class CardItemData : public ProgramTreeItemData
{
	wxString mCardName;

public:
	CardItemData(ProgramTreeCtrl *inTreeCtrl, const wxString &inCardName);
	virtual void OnLeftDClick(wxMouseEvent& event);
};

CardItemData::CardItemData(ProgramTreeCtrl *inTreeCtrl,
						   const wxString &inCardName)
	: ProgramTreeItemData(inTreeCtrl), mCardName(inCardName)
{
}

void CardItemData::OnLeftDClick(wxMouseEvent& event)
{
	if (TInterpreter::HaveInstance())
	{
		TInterpreter *interp = TInterpreter::GetInstance();
		if (interp->IsValidCard(mCardName.mb_str()))
			interp->JumpToCardByName(mCardName.mb_str());
	}
}


//=========================================================================
//  BackgroundItemData
//=========================================================================

class BackgroundItemData : public ViewItemData
{
public:
	BackgroundItemData(ProgramTreeCtrl *inTreeCtrl)
		: ViewItemData(inTreeCtrl) {}

	virtual void ObjectChanged();
	virtual void ObjectDeleted();
};

void BackgroundItemData::ObjectChanged()
{
	wxASSERT(GetId());
	// XXX - Object creation protocol is totally broken, so we need to
	// make sure Initialize() has been called on our object.
	// TODO - Rename HaveKey to HasKey.
	if (GetObject()->HaveKey("name"))
	{
		wxString name(GetObject()->GetString("name").c_str());
		GetTree()->SetItemText(GetId(), name);
	}
}

void BackgroundItemData::ObjectDeleted()
{
}


//=========================================================================
//  BackgroundListItemData
//=========================================================================

class BackgroundListItemData : public ViewItemData
{
	model::Object *GetItemObject(wxTreeItemId id);

public:
	BackgroundListItemData(ProgramTreeCtrl *inTreeCtrl)
		: ViewItemData(inTreeCtrl) {}

	virtual void ObjectChanged();
	virtual void ObjectDeleted();
};

model::Object *BackgroundListItemData::GetItemObject(wxTreeItemId id)
{
	BackgroundItemData *data =
		dynamic_cast<BackgroundItemData*>(GetTree()->GetItemData(id));
	wxASSERT(data);
	return data->GetObject();
}

void BackgroundListItemData::ObjectChanged()
{
	// Get our list of backgrounds.
	model::List *backgrounds =
		model::cast<TamaleProgram>(GetObject())->GetBackgrounds();

	// This is messy--we need to update the tree's list of backgrounds to
	// match the list in our model without changing the contents of the
	// wxTreeCtrl more than absolutely necessary.  We use a Largest
	// Common Subsequence algorithm to implement a primitive "diff"
	// between the two lists.
	typedef std::vector<model::Object*> ObjectVector;
	ObjectVector view_items, model_items, lcs;

	// Extract the Objects from the Views in our tree.
	ProgramTreeCtrl *tree = GetTree();
	wxTreeItemId id = GetId();
	long cookie;
	for (id = tree->GetFirstChild(GetId(), cookie); id;
		 id = tree->GetNextChild(GetId(), cookie))
		view_items.push_back(GetItemObject(id));

	// Extract the Objects from our Model.
	size_t model_count = backgrounds->GetSize();
	for (int i = 0; i < model_count; i++)
		model_items.push_back(model::cast<model::Object>(backgrounds->Get(i)));

	// Calculate our Largest Common Subsequence.  This is essentially
	// the list of "unchanged" elements.
	LargestCommonSubsequence<model::Object*>(view_items, model_items, lcs);

	// Delete any objects which have disappeared from our tree.
	ObjectVector::iterator lcs_iter = lcs.begin();
	for (id = tree->GetFirstChild(GetId(), cookie); id;
		 id = tree->GetNextChild(GetId(), cookie))
		if (lcs_iter != lcs.end() && GetItemObject(id) == *lcs_iter)
			++lcs_iter;
		else
			tree->Delete(id);

	// Insert any objects which have been added to our tree.
	lcs_iter = lcs.begin();
	ObjectVector::iterator model_iter = model_items.begin();
	for (size_t view_index = 0;
		 model_iter != model_items.end();
		 ++model_iter, ++view_index)
	{
		if (lcs_iter != lcs.end() && *model_iter == *lcs_iter)
			++lcs_iter;
		else
		{
			ViewItemData *data = new BackgroundItemData(tree);
			tree->InsertItem(GetId(), view_index, "", -1, -1, data);
			data->SetObject(*model_iter);
		}
	}
}

void BackgroundListItemData::ObjectDeleted()
{
}


//=========================================================================
//  TamaleProgramMenu
//=========================================================================

class TamaleProgramMenu : public wxMenu
{
	DECLARE_EVENT_TABLE();

	wxWindow *mParent;
	model::Object *mObject;

    void OnProperties(wxCommandEvent &inEvent);	

public:
	TamaleProgramMenu(wxWindow *inParent, model::Object *inObject);
};

BEGIN_EVENT_TABLE(TamaleProgramMenu, wxMenu)
    EVT_MENU(FIVEL_PROPERTIES, TamaleProgramMenu::OnProperties)
END_EVENT_TABLE()

TamaleProgramMenu::TamaleProgramMenu(wxWindow *inParent, model::Object *inObject)
{
	mParent = inParent;
	mObject = inObject;
	Append(FIVEL_PROPERTIES, "Properties...",
		   "Edit the properties for this program.");
}

void TamaleProgramMenu::OnProperties(wxCommandEvent &inEvent)
{
	ProgramPropDlg prop_dlg(mParent, mObject);
	prop_dlg.ShowModal();
}


//=========================================================================
//  TamaleProgramItemData
//=========================================================================

class TamaleProgramItemData : public ViewItemData
{
public:
	TamaleProgramItemData(ProgramTreeCtrl *inTreeCtrl)
		: ViewItemData(inTreeCtrl) {}

	virtual void OnRightDown(wxMouseEvent& event);

	virtual void ObjectChanged();
	virtual void ObjectDeleted();
};

void TamaleProgramItemData::OnRightDown(wxMouseEvent& event)
{
	TamaleProgramMenu popup(GetTree(), GetObject());
	GetTree()->PopupMenu(&popup, event.GetPosition());
}

void TamaleProgramItemData::ObjectChanged()
{
	wxASSERT(GetId());
	wxString name(GetObject()->GetString("name").c_str());
	GetTree()->SetItemText(GetId(), "Program '" + name + "'");
}

void TamaleProgramItemData::ObjectDeleted()
{
}


//=========================================================================
//  ProgramTreeCtrl
//=========================================================================

BEGIN_EVENT_TABLE(ProgramTreeCtrl, wxTreeCtrl)
    EVT_LEFT_DCLICK(ProgramTreeCtrl::OnLeftDClick)
    EVT_RIGHT_DOWN(ProgramTreeCtrl::OnRightDown)
END_EVENT_TABLE()

ProgramTreeCtrl::ProgramTreeCtrl(wxWindow *inParent, int id)
	: wxTreeCtrl(inParent, id)
{
	BuildIconList();
}

void ProgramTreeCtrl::SetIcon(wxTreeItemId id, int closed_icon, int open_icon)
{
	SetItemImage(id, closed_icon, wxTreeItemIcon_Normal);
	SetItemImage(id, closed_icon, wxTreeItemIcon_Selected);
	SetItemImage(id, open_icon, wxTreeItemIcon_Expanded);
	SetItemImage(id, open_icon, wxTreeItemIcon_SelectedExpanded);
}

void ProgramTreeCtrl::BuildIconList()
{
	// This should match the enumeration of icons in our class
	// declaration.
	wxImageList *images = new wxImageList(16, 16, TRUE);
	images->Add(wxICON(ic_card));
	images->Add(wxICON(ic_document));
	images->Add(wxICON(ic_folder_closed));
	images->Add(wxICON(ic_folder_open));
	AssignImageList(images);
}

void ProgramTreeCtrl::OnLeftDClick(wxMouseEvent& event)
{
    wxTreeItemId id = HitTest(event.GetPosition());
    if (id)
	{
		ProgramTreeItemData *data =
			dynamic_cast<ProgramTreeItemData*>(GetItemData(id));
		if (data)
			data->OnLeftDClick(event);
	}
}

void ProgramTreeCtrl::OnRightDown(wxMouseEvent& event)
{
    wxTreeItemId id = HitTest(event.GetPosition());
    if (id)
	{
		ProgramTreeItemData *data =
			dynamic_cast<ProgramTreeItemData*>(GetItemData(id));
		if (data)
			data->OnRightDown(event);
	}
}


//=========================================================================
//  ProgramTree Methods
//=========================================================================

BEGIN_EVENT_TABLE(ProgramTree, wxSashLayoutWindow)
END_EVENT_TABLE()

ProgramTree::ProgramTree(StageFrame *inStageFrame, int inID)
	: wxSashLayoutWindow(inStageFrame, inID),
	  mHaveLastHighlightedItem(false)
{
	// Set up our tree control.
	mTree = new ProgramTreeCtrl(this, FIVEL_PROGRAM_TREE_CTRL);
	mRootID = mCardsID = mBackgroundsID;

	// Set our minimum sash width.
	SetMinimumSizeX(MINIMUM_WIDTH);
    SetDefaultWidth(MINIMUM_WIDTH);
}

void ProgramTree::RegisterDocument(Document *inDocument)
{
	// Set up our root node.
	mRootID = mTree->AddRoot("Program");
	mTree->SetIcon(mRootID, ProgramTreeCtrl::ICON_DOCUMENT,
				   ProgramTreeCtrl::ICON_DOCUMENT);
	TamaleProgramItemData *item_data = new TamaleProgramItemData(mTree);
	mTree->SetItemData(mRootID, item_data);
	item_data->SetObject(inDocument->GetRoot());

	// Set up some other nodes.
	mCardsID = mTree->AppendItem(mRootID, "Cards");
	mTree->SetIcon(mCardsID, ProgramTreeCtrl::ICON_FOLDER_CLOSED,
				   ProgramTreeCtrl::ICON_FOLDER_OPEN);
	mBackgroundsID = mTree->AppendItem(mRootID, "Backgrounds");
	mTree->SetIcon(mBackgroundsID, ProgramTreeCtrl::ICON_FOLDER_CLOSED,
				   ProgramTreeCtrl::ICON_FOLDER_OPEN);
	ViewItemData *bg_data = new BackgroundListItemData(mTree);
	mTree->SetItemData(mBackgroundsID, bg_data);
	bg_data->SetObject(inDocument->GetRoot());
}

void ProgramTree::RegisterCard(const wxString &inName)
{
	// Check to make sure we don't already have a card by this name.
	wxASSERT(mCardMap.find(inName.mb_str()) == mCardMap.end());

	// Insert the card into our tree.
	wxTreeItemId id = mTree->AppendItem(mCardsID, inName);
	mTree->SetItemData(id, new CardItemData(mTree, inName));
	mTree->SetIcon(id, ProgramTreeCtrl::ICON_CARD,
				   ProgramTreeCtrl::ICON_CARD);

	// Record the card in our map.
	mCardMap.insert(ItemMap::value_type(inName.mb_str(), id));
}

void ProgramTree::SetDefaultWidth(int inWidth)
{
	SetDefaultSize(wxSize(inWidth, 0 /* unused */));
}

void ProgramTree::NotifyScriptReload()
{
	mCardMap.clear();
	mTree->CollapseAndReset(mCardsID);
}

void ProgramTree::NotifyEnterCard()
{
	// Look up the ID corresponding to this card.
	ASSERT(TInterpreter::HaveInstance());
	std::string card = TInterpreter::GetInstance()->CurCardName();
	ItemMap::iterator found = mCardMap.find(card);
	wxASSERT(found != mCardMap.end());

	// Move the highlighting to the appropriate card.
	if (mHaveLastHighlightedItem)
		mTree->SetItemBold(mLastHighlightedItem, FALSE);
	mTree->SetItemBold(found->second);
	mHaveLastHighlightedItem = true;
	mLastHighlightedItem = found->second;
	mTree->EnsureVisible(found->second);
}
