// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include <wx/treectrl.h>
#include <wx/laywin.h>

#include "TCommon.h"
#include "TInterpreter.h"

#include <map>
#include <string>

#include "AppGlobals.h"
#include "Stage.h"
#include "ProgramTree.h"
#include "Model.h"
#include "doc/Document.h"


//=========================================================================
//  ProgramTreeCtrl
//=========================================================================

class ProgramTreeCtrl : public wxTreeCtrl
{
	DECLARE_EVENT_TABLE()

public:
	ProgramTreeCtrl(wxWindow *inParent, int id);

private:
	void OnLeftDClick(wxMouseEvent& event);
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
//  TamaleProgramItemData
//=========================================================================

class TamaleProgramItemData : public ViewItemData
{
public:
	TamaleProgramItemData(ProgramTreeCtrl *inTreeCtrl)
		: ViewItemData(inTreeCtrl) {}

	virtual void ObjectChanged();
	virtual void ObjectDeleted();
};

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
END_EVENT_TABLE()

ProgramTreeCtrl::ProgramTreeCtrl(wxWindow *inParent, int id)
	: wxTreeCtrl(inParent, id)
{
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
	TamaleProgramItemData *item_data = new TamaleProgramItemData(mTree);
	mTree->SetItemData(mRootID, item_data);
	item_data->SetObject(inDocument->GetRoot());

	// Set up some other nodes.
	mCardsID = mTree->AppendItem(mRootID, "Cards");
	mBackgroundsID = mTree->AppendItem(mRootID, "Backgrounds");
}

void ProgramTree::RegisterCard(const wxString &inName)
{
	// Check to make sure we don't already have a card by this name.
	wxASSERT(mCardMap.find(inName.mb_str()) == mCardMap.end());

	// Insert the card into our tree.
	wxTreeItemId id = mTree->AppendItem(mCardsID, inName);
	mTree->SetItemData(id, new CardItemData(mTree, inName));

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
	mTree->SetItemText(mRootID, "Program");
	mTree->CollapseAndReset(mCardsID);
	mTree->CollapseAndReset(mBackgroundsID);
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
