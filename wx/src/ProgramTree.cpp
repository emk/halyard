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

#include "TamaleHeaders.h"
#include <wx/treectrl.h>
#include <wx/laywin.h>
#include <wx/imaglist.h>
#include <wx/dnd.h>

#include "TInterpreter.h"
#include "TVectorDiff.h"

#include "AppGlobals.h"
#include "FiveLApp.h"
#include "Stage.h"
#include "StageFrame.h"
#include "ProgramTree.h"
#include "Model.h"
#include "ModelView.h"
#include "doc/Document.h"
#include "doc/TamaleProgram.h"
#include "dlg/ProgramPropDlg.h"

USING_NAMESPACE_FIVEL

class ProgramTreeItemData;


//=========================================================================
//  ProgramTreeCtrl
//=========================================================================

/// Tree widget showing structure of Tamale script.
class ProgramTreeCtrl : public wxTreeCtrl
{
	DECLARE_EVENT_TABLE()

	ProgramTreeItemData *mDragItemData;

	ProgramTreeItemData *GetProgramTreeItemData(wxTreeItemId inId);
	ProgramTreeItemData *GetProgramTreeItemData(wxMouseEvent &inEvent);
	ProgramTreeItemData *GetProgramTreeItemData(wxTreeEvent &inEvent);

public:
	ProgramTreeCtrl(wxWindow *inParent);

	//////////
	/// Any of these icons may be used by nodes in the ProgramTree.
	///
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
	void OnBeginLabelEdit(wxTreeEvent &event);
	void OnEndLabelEdit(wxTreeEvent &event);
	void OnBeginDrag(wxTreeEvent& event);
	void OnEndDrag(wxTreeEvent& event);
	void OnMouseMoved(wxMouseEvent& event);
};	


//=========================================================================
//  ProgramTreeItemData
//=========================================================================

///  This class respresents a "smart" node in our ProgramTreeCtrl.  Most
///  node-specific events will be passed to a subclass of
///  ProgramTreeItemData by our event handlers.
class ProgramTreeItemData : public wxTreeItemData
{
	ProgramTreeCtrl *mTreeCtrl;

public:
	ProgramTreeItemData(ProgramTreeCtrl *inTreeCtrl);
	ProgramTreeCtrl *GetTree() { return mTreeCtrl; }

	virtual void OnLeftDClick(wxMouseEvent& event) {}
	virtual void OnRightDown(wxMouseEvent& event) {}
	virtual void OnBeginLabelEdit(wxTreeEvent &event);
	virtual void OnEndLabelEdit(wxTreeEvent &event) {}

	virtual bool CanBeDragged() { return false; }
	virtual bool CanAcceptDrag(ProgramTreeItemData *inItem) { return false; }
	virtual void DragDone(ProgramTreeItemData *inItem) {}
};

ProgramTreeItemData::ProgramTreeItemData(ProgramTreeCtrl *inTreeCtrl)
	: mTreeCtrl(inTreeCtrl)
{
	ASSERT(mTreeCtrl != NULL);
}

void ProgramTreeItemData::OnBeginLabelEdit(wxTreeEvent &event)
{
	// By default, do not allow nodes in our tree to be renamed.
	event.Veto();
}


//=========================================================================
//  ViewItemData
//=========================================================================

/// An object in our ProgramTreeCtrl which listens to our Document model.
class ViewItemData : public ProgramTreeItemData, public model::View
{
public:
	ViewItemData(ProgramTreeCtrl *inTreeCtrl)
		: ProgramTreeItemData(inTreeCtrl) {}	
};


//=========================================================================
//  SequenceItemData
//=========================================================================

/// Sequences of cards can be nested within each other.
class SequenceItemData : public ProgramTreeItemData
{
public:
	SequenceItemData(ProgramTreeCtrl *inTreeCtrl);
};

SequenceItemData::SequenceItemData(ProgramTreeCtrl *inTreeCtrl)
	: ProgramTreeItemData(inTreeCtrl)
{
}


//=========================================================================
//  CardItemData
//=========================================================================

/// Representation of a card in our ProgramTreeCtrl.
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
	Stage *stage = wxGetApp().GetStage();
	if (stage->CanJump())
		stage->TryJumpTo(mCardName);
}


//=========================================================================
//  BackgroundItemData
//=========================================================================

/// Representation of a background in our ProgramTreeCtrl.
class BackgroundItemData : public ViewItemData
{
public:
	BackgroundItemData(ProgramTreeCtrl *inTreeCtrl)
		: ViewItemData(inTreeCtrl) {}

	virtual void OnBeginLabelEdit(wxTreeEvent &event);
	virtual void OnEndLabelEdit(wxTreeEvent &event);

	virtual bool CanBeDragged() { return true; }
	virtual bool CanAcceptDrag(ProgramTreeItemData *inItem);
	virtual void DragDone(ProgramTreeItemData *inItem);

	virtual void ObjectChanged();
	virtual void ObjectDeleted();
};

void BackgroundItemData::OnBeginLabelEdit(wxTreeEvent &event)
{
	// Override our default Veto() action.
}

void BackgroundItemData::OnEndLabelEdit(wxTreeEvent &event)
{
	if (!event.IsEditCancelled())
		GetObject()->SetString("name", event.GetLabel().mb_str());
}

bool BackgroundItemData::CanAcceptDrag(ProgramTreeItemData *inItem)
{
	return dynamic_cast<BackgroundItemData*>(inItem) ? true : false;
}

void BackgroundItemData::DragDone(ProgramTreeItemData *inItem)
{
	BackgroundItemData *source = dynamic_cast<BackgroundItemData*>(inItem);
	wxASSERT(source);
	wxString src_name = source->GetObject()->GetString("name").c_str();
	wxString dst_name = GetObject()->GetString("name").c_str();
	::wxLogError("Dragged \'" + src_name + "\' to \'" + dst_name + "\'");
}

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

/// Folder containing all backgrounds in our ProgramTreeCtrl.
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
	for (size_t i = 0; i < model_count; i++)
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

/// Right-click menu for the TamaleProgramItemData in our ProgramTreeCtrl.
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

/// Representation of the entire Tamale script in our ProgramTreeCtrl.
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
//  ProgramTreeDropTarget
//=========================================================================

/*
class ProgramTreeDropTarget : public wxTextDropTarget
{
	ProgramTreeCtrl *mTree;

public:
	ProgramTreeDropTarget(ProgramTreeCtrl *inTree) : mTree(inTree) {}

	virtual bool OnDropText(int x, int y, const wxString &data);
};

bool ProgramTreeDropTarget::OnDropText(int x, int y, const wxString &data)
{
	wxLogError("Dropped " + data + ".");
	return TRUE; // Don't veto the operation.
}
*/


//=========================================================================
//  ProgramTreeCtrl
//=========================================================================

BEGIN_EVENT_TABLE(ProgramTreeCtrl, wxTreeCtrl)
    EVT_LEFT_DCLICK(ProgramTreeCtrl::OnLeftDClick)
    EVT_RIGHT_DOWN(ProgramTreeCtrl::OnRightDown)
	EVT_TREE_BEGIN_LABEL_EDIT(FIVEL_PROGRAM_TREE_CTRL,
							  ProgramTreeCtrl::OnBeginLabelEdit)
	EVT_TREE_END_LABEL_EDIT(FIVEL_PROGRAM_TREE_CTRL,
							ProgramTreeCtrl::OnEndLabelEdit)
	EVT_TREE_BEGIN_DRAG(FIVEL_PROGRAM_TREE_CTRL,
						ProgramTreeCtrl::OnBeginDrag)
	EVT_TREE_END_DRAG(FIVEL_PROGRAM_TREE_CTRL,
					  ProgramTreeCtrl::OnEndDrag)
    EVT_MOTION(ProgramTreeCtrl::OnMouseMoved)
END_EVENT_TABLE()

ProgramTreeCtrl::ProgramTreeCtrl(wxWindow *inParent)
	: wxTreeCtrl(inParent, FIVEL_PROGRAM_TREE_CTRL,
				 wxDefaultPosition, wxDefaultSize,
				 wxTR_DEFAULT_STYLE|wxTR_EDIT_LABELS),
	  mDragItemData(NULL)
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

ProgramTreeItemData *
ProgramTreeCtrl::GetProgramTreeItemData(wxTreeItemId inId)
{
	return dynamic_cast<ProgramTreeItemData*>(GetItemData(inId));
}

ProgramTreeItemData *
ProgramTreeCtrl::GetProgramTreeItemData(wxMouseEvent &inEvent)
{
    wxTreeItemId id = HitTest(inEvent.GetPosition());
	return id ? GetProgramTreeItemData(id) : NULL;
}

ProgramTreeItemData *
ProgramTreeCtrl::GetProgramTreeItemData(wxTreeEvent &inEvent)
{
	return GetProgramTreeItemData(inEvent.GetItem());
}

void ProgramTreeCtrl::OnLeftDClick(wxMouseEvent& event)
{
	ProgramTreeItemData *data = GetProgramTreeItemData(event);
	if (data)
		data->OnLeftDClick(event);
}

void ProgramTreeCtrl::OnRightDown(wxMouseEvent& event)
{
	ProgramTreeItemData *data = GetProgramTreeItemData(event);
	if (data)
		data->OnRightDown(event);
}

void ProgramTreeCtrl::OnBeginLabelEdit(wxTreeEvent &event)
{
	ProgramTreeItemData *data = GetProgramTreeItemData(event);
	if (data)
		data->OnBeginLabelEdit(event);	
}

void ProgramTreeCtrl::OnEndLabelEdit(wxTreeEvent &event)
{
	ProgramTreeItemData *data = GetProgramTreeItemData(event);
	if (data)
		data->OnEndLabelEdit(event);	
}

void ProgramTreeCtrl::OnBeginDrag(wxTreeEvent& event)
{
	ProgramTreeItemData *data = GetProgramTreeItemData(event);
	if (data && data->CanBeDragged())
	{
		event.Allow();
		mDragItemData = data;
	}
}

void ProgramTreeCtrl::OnEndDrag(wxTreeEvent& event)
{
	ProgramTreeItemData *data = GetProgramTreeItemData(event);
	if (data && data->CanAcceptDrag(mDragItemData))
		data->DragDone(mDragItemData);
	mDragItemData = NULL;
}

void ProgramTreeCtrl::OnMouseMoved(wxMouseEvent& event)
{
	if (!mDragItemData)
		SetCursor(*wxSTANDARD_CURSOR);
	else
	{
		ProgramTreeItemData *data = GetProgramTreeItemData(event);
		if (data && data != mDragItemData &&
			data->CanAcceptDrag(mDragItemData))
			SetCursor(*wxSTANDARD_CURSOR);
		else
			SetCursor(wxCursor(wxCURSOR_NO_ENTRY));
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
	mTree = new ProgramTreeCtrl(this);
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

wxTreeItemId ProgramTree::FindParentContainer(const std::string &inName,
											  std::string &outLocalName)
{
	std::string::size_type slashpos = inName.rfind('/');
	if (slashpos == std::string::npos)
	{
		// We didn't find a slash anywhere in the string, so put it at the
		// top level and call it done. 
		outLocalName = inName;
		return mCardsID;
	}
	else if (slashpos == 0 || slashpos == inName.size() - 1)
	{
		// We don't like this string.
		gLog.Error("Illegal card name: \"%s", inName.c_str());
		outLocalName = inName;
		return mCardsID;
	}
	else
	{
		// Extract the local portion of the name for use by our caller.
		outLocalName = std::string(inName, slashpos + 1, std::string::npos);

		// Extract the parent sequence name.
		std::string parent_name(inName, 0, slashpos);

		// Either find or create the parent sequence.
		wxASSERT(mCardMap.find(parent_name) == mCardMap.end());
		ItemMap::iterator found = mSequenceMap.find(parent_name);
		if (found != mSequenceMap.end())
			return found->second;
		else
		{
			// We're going to have to create it.  First, find the grandparent.
			std::string parent_local_name;
			wxTreeItemId grandparent_id =
				FindParentContainer(parent_name, parent_local_name);

			// Now, create the parent.
			wxTreeItemId parent_id =
				mTree->AppendItem(grandparent_id, parent_local_name.c_str());
			mTree->SetItemData(parent_id, new SequenceItemData(mTree));
			mTree->SetIcon(parent_id, ProgramTreeCtrl::ICON_FOLDER_CLOSED,
						   ProgramTreeCtrl::ICON_FOLDER_OPEN);

			// Add the parent to our map so we can find it later.
			mSequenceMap.insert(ItemMap::value_type(parent_name, parent_id));
			return parent_id;
		}
	}
}

void ProgramTree::RegisterCard(const wxString &inName)
{
	// Check to make sure we don't already have a card by this name.
	wxASSERT(mCardMap.find(inName.mb_str()) == mCardMap.end());

	// Insert the card into our tree.
	std::string local_name;
	wxTreeItemId parent_id = FindParentContainer(inName.mb_str(), local_name);
	wxTreeItemId id = mTree->AppendItem(parent_id, local_name.c_str());
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

void ProgramTree::NotifyReloadScriptStarting()
{
	mCardMap.clear();
	mSequenceMap.clear();
	mTree->CollapseAndReset(mCardsID);
}

void ProgramTree::NotifyEnterCard(const wxString &inName)
{
	// Look up the ID corresponding to this card.
	ItemMap::iterator found = mCardMap.find(inName.mb_str());
	wxASSERT(found != mCardMap.end());

	// Move the highlighting to the appropriate card.
	if (mHaveLastHighlightedItem)
		mTree->SetItemBold(mLastHighlightedItem, FALSE);
	mTree->SetItemBold(found->second);
	mHaveLastHighlightedItem = true;
	mLastHighlightedItem = found->second;
	mTree->EnsureVisible(found->second);
}
