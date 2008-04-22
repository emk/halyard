// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

#include "AppHeaders.h"
#include <wx/treectrl.h>
#include <wx/laywin.h>
#include <wx/dnd.h>

#include "TInterpreter.h"
#include "TVectorDiff.h"

#include "AppGlobals.h"
#include "HalyardApp.h"
#include "Stage.h"
#include "StageFrame.h"
#include "CustomTreeCtrl.h"
#include "ProgramTree.h"
#include "Model.h"
#include "ModelView.h"
#include "doc/Document.h"
#include "doc/HalyardProgram.h"
#include "dlg/ProgramPropDlg.h"

using namespace Halyard;

class ProgramTreeItemData;


//=========================================================================
//  ProgramTreeCtrl
//=========================================================================

/// Tree widget showing structure of Halyard script.
class ProgramTreeCtrl : public CustomTreeCtrl
{
public:
	ProgramTreeCtrl(wxWindow *inParent);
};	


//=========================================================================
//  ViewItemData
//=========================================================================

/// An object in our ProgramTreeCtrl which listens to our Document model.
class ViewItemData : public CustomTreeItemData, public model::View
{
public:
	ViewItemData(ProgramTreeCtrl *inTreeCtrl)
		: CustomTreeItemData(inTreeCtrl) {}	
};


//=========================================================================
//  SequenceItemData
//=========================================================================

/// Sequences of cards can be nested within each other.
class SequenceItemData : public CustomTreeItemData
{
public:
	SequenceItemData(ProgramTreeCtrl *inTreeCtrl);
};

SequenceItemData::SequenceItemData(ProgramTreeCtrl *inTreeCtrl)
	: CustomTreeItemData(inTreeCtrl)
{
}


//=========================================================================
//  CardItemData
//=========================================================================

/// Representation of a card in our ProgramTreeCtrl.
class CardItemData : public CustomTreeItemData
{
	wxString mCardName;

public:
	CardItemData(ProgramTreeCtrl *inTreeCtrl, const wxString &inCardName);
	virtual void OnLeftDClick(wxMouseEvent& event);
};

CardItemData::CardItemData(ProgramTreeCtrl *inTreeCtrl,
						   const wxString &inCardName)
	: CustomTreeItemData(inTreeCtrl), mCardName(inCardName)
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
//  Our usual policy is to delete unused code, not comment it out.
//  However, the class below is the only remaining example code for quite a
//  few important APIs which have been fairly-well debugged and which we
//  will need in our eventual GUI editor.  So for now, we're going to leave
//  this in.  But if this code remains here indefinitely, please feel free
//  to delete it--and backtrack through the APIs it uses, digging them out
//  as well.

/// Representation of a background in our ProgramTreeCtrl.
// class BackgroundItemData : public ViewItemData
// {
// public:
//     BackgroundItemData(ProgramTreeCtrl *inTreeCtrl)
//         : ViewItemData(inTreeCtrl) {}

//     virtual void OnBeginLabelEdit(wxTreeEvent &event);
//     virtual void OnEndLabelEdit(wxTreeEvent &event);

//     virtual bool CanBeDragged() { return true; }
//     virtual bool CanAcceptDrag(CustomTreeItemData *inItem);
//     virtual void DragDone(CustomTreeItemData *inItem);

//     virtual void ObjectChanged();
//     virtual void ObjectDeleted();
// };

// void BackgroundItemData::OnBeginLabelEdit(wxTreeEvent &event)
// {
//     // Override our default Veto() action.
// }

// void BackgroundItemData::OnEndLabelEdit(wxTreeEvent &event)
// {
//     if (!event.IsEditCancelled())
//         GetObject()->SetString("name", event.GetLabel().mb_str());
// }

// bool BackgroundItemData::CanAcceptDrag(CustomTreeItemData *inItem)
// {
//     return dynamic_cast<BackgroundItemData*>(inItem) ? true : false;
// }

// void BackgroundItemData::DragDone(CustomTreeItemData *inItem)
// {
//     BackgroundItemData *source = dynamic_cast<BackgroundItemData*>(inItem);
//     wxASSERT(source);
//     wxString src_name = source->GetObject()->GetString("name").c_str();
//     wxString dst_name = GetObject()->GetString("name").c_str();
//     ::wxLogError("Dragged \'" + src_name + "\' to \'" + dst_name + "\'");
// }

// void BackgroundItemData::ObjectChanged()
// {
//     wxASSERT(GetId());
//     // XXX - Object creation protocol is totally broken, so we need to
//     // make sure Initialize() has been called on our object.
//     // TODO - Rename HaveKey to HasKey.
//     if (GetObject()->HaveKey("name"))
//     {
//         wxString name(GetObject()->GetString("name").c_str());
//         GetTree()->SetItemText(GetId(), name);
//     }
// }

// void BackgroundItemData::ObjectDeleted()
// {
// }


//=========================================================================
//  BackgroundListItemData
//=========================================================================

// /// Folder containing all backgrounds in our ProgramTreeCtrl.
// class BackgroundListItemData : public ViewItemData
// {
//     model::Object *GetItemObject(wxTreeItemId id);

// public:
//     BackgroundListItemData(ProgramTreeCtrl *inTreeCtrl)
//         : ViewItemData(inTreeCtrl) {}

//     virtual void ObjectChanged();
//     virtual void ObjectDeleted();
// };

// model::Object *BackgroundListItemData::GetItemObject(wxTreeItemId id)
// {
//     BackgroundItemData *data =
//         dynamic_cast<BackgroundItemData*>(GetTree()->GetItemData(id));
//     wxASSERT(data);
//     return data->GetObject();
// }

// void BackgroundListItemData::ObjectChanged()
// {
//     // Get our list of backgrounds.
//     model::List *backgrounds =
//         model::cast<HalyardProgram>(GetObject())->GetBackgrounds();

//     // This is messy--we need to update the tree's list of backgrounds to
//     // match the list in our model without changing the contents of the
//     // wxTreeCtrl more than absolutely necessary.  We use a Largest
//     // Common Subsequence algorithm to implement a primitive "diff"
//     // between the two lists.
//     typedef std::vector<model::Object*> ObjectVector;
//     ObjectVector view_items, model_items, lcs;

//     // Extract the Objects from the Views in our tree.
//     ProgramTreeCtrl *tree = dynamic_cast<ProgramTreeCtrl*>(GetTree());
//     wxTreeItemId id = GetId();
//     wxTreeItemIdValue cookie;
//     for (id = tree->GetFirstChild(GetId(), cookie); id;
//          id = tree->GetNextChild(GetId(), cookie))
//         view_items.push_back(GetItemObject(id));

//     // Extract the Objects from our Model.
//     size_t model_count = backgrounds->GetSize();
//     for (size_t i = 0; i < model_count; i++)
//         model_items.push_back(model::cast<model::Object>(backgrounds->Get(i)));

//     // Calculate our Largest Common Subsequence.  This is essentially
//     // the list of "unchanged" elements.
//     LargestCommonSubsequence<model::Object*>(view_items, model_items, lcs);

//     // Delete any objects which have disappeared from our tree.
//     ObjectVector::iterator lcs_iter = lcs.begin();
//     for (id = tree->GetFirstChild(GetId(), cookie); id;
//          id = tree->GetNextChild(GetId(), cookie))
//         if (lcs_iter != lcs.end() && GetItemObject(id) == *lcs_iter)
//             ++lcs_iter;
//         else
//             tree->Delete(id);

//     // Insert any objects which have been added to our tree.
//     lcs_iter = lcs.begin();
//     ObjectVector::iterator model_iter = model_items.begin();
//     for (size_t view_index = 0;
//          model_iter != model_items.end();
//          ++model_iter, ++view_index)
//     {
//         if (lcs_iter != lcs.end() && *model_iter == *lcs_iter)
//             ++lcs_iter;
//         else
//         {
//             ViewItemData *data = new BackgroundItemData(tree);
//             tree->InsertItem(GetId(), view_index, "", -1, -1, data);
//             data->SetObject(*model_iter);
//         }
//     }
// }

// void BackgroundListItemData::ObjectDeleted()
// {
// }


//=========================================================================
//  HalyardProgramMenu
//=========================================================================

/// Right-click menu for the HalyardProgramItemData in our ProgramTreeCtrl.
class HalyardProgramMenu : public wxMenu
{
	DECLARE_EVENT_TABLE();

	wxWindow *mParent;
	model::Object *mObject;

    void OnProperties(wxCommandEvent &inEvent);	

public:
	HalyardProgramMenu(wxWindow *inParent, model::Object *inObject);
};

BEGIN_EVENT_TABLE(HalyardProgramMenu, wxMenu)
    EVT_MENU(HALYARD_PROPERTIES, HalyardProgramMenu::OnProperties)
END_EVENT_TABLE()

HalyardProgramMenu::HalyardProgramMenu(wxWindow *inParent, model::Object *inObject)
{
	mParent = inParent;
	mObject = inObject;
	Append(HALYARD_PROPERTIES, wxT("Properties..."),
		   wxT("Edit the properties for this program."));
}

void HalyardProgramMenu::OnProperties(wxCommandEvent &inEvent)
{
	ProgramPropDlg prop_dlg(mParent, mObject);
	prop_dlg.ShowModal();
}


//=========================================================================
//  HalyardProgramItemData
//=========================================================================

/// Representation of the entire Halyard script in our ProgramTreeCtrl.
class HalyardProgramItemData : public ViewItemData
{
public:
	HalyardProgramItemData(ProgramTreeCtrl *inTreeCtrl)
		: ViewItemData(inTreeCtrl) {}

	virtual void OnRightDown(wxMouseEvent& event);

	virtual void ObjectChanged();
	virtual void ObjectDeleted();
};

void HalyardProgramItemData::OnRightDown(wxMouseEvent& event)
{
	HalyardProgramMenu popup(GetTree(), GetObject());
	GetTree()->PopupMenu(&popup, event.GetPosition());
}

void HalyardProgramItemData::ObjectChanged()
{
	wxASSERT(GetId());
	wxString name(GetObject()->GetString("name").c_str(), wxConvLocal);
	GetTree()->SetItemText(GetId(), wxT("Program '") + name + wxT("'"));
}

void HalyardProgramItemData::ObjectDeleted()
{
}


//=========================================================================
//  ProgramTreeCtrl
//=========================================================================

ProgramTreeCtrl::ProgramTreeCtrl(wxWindow *inParent)
	: CustomTreeCtrl(inParent, HALYARD_PROGRAM_TREE_CTRL,
                     wxDefaultPosition, wxDefaultSize,
                     wxTR_DEFAULT_STYLE|wxTR_EDIT_LABELS)
{
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

	// Set our minimum sash width.
	SetMinimumSizeX(MINIMUM_WIDTH);
    SetDefaultWidth(MINIMUM_WIDTH);
}

void ProgramTree::RegisterDocument(Document *inDocument)
{
	// Set up our root node.
	mRootID = mTree->AddRoot(wxT("Program"));
	mTree->SetIcon(mRootID, ProgramTreeCtrl::ICON_DOCUMENT,
				   ProgramTreeCtrl::ICON_DOCUMENT);
	HalyardProgramItemData *item_data = new HalyardProgramItemData(mTree);
	mTree->SetItemData(mRootID, item_data);
	item_data->SetObject(inDocument->GetRoot());

	// Set up some other nodes.
	mCardsID = mTree->AppendItem(mRootID, wxT("Cards"));
	mTree->SetIcon(mCardsID, ProgramTreeCtrl::ICON_FOLDER_CLOSED,
				   ProgramTreeCtrl::ICON_FOLDER_OPEN);
}

wxTreeItemId ProgramTree::FindParentContainer(const std::string &inName,
											  std::string &outLocalName)
{
    ASSERT(mCardsID.IsOk());
    // Look backwards through the string for slashes, to separate the
    // node name from the parent name.
	std::string::size_type slashpos = inName.rfind('/');
	if (slashpos == 0)
	{
		// The only slash is at the beginning of the string,
        // indicating we're a child of the root node.  Strip off the
        // slash for the local name, and return mCardsID to put us at
        // the root of the hierarchy.
		outLocalName = std::string(inName, 1, std::string::npos);
		return mCardsID;
	}
	else if (slashpos == std::string::npos || slashpos == inName.size() - 1)
	{
		// We don't like this string.
		gLog.Error("Illegal card name: \"%s\"", inName.c_str());
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
				mTree->AppendItem(grandparent_id,
                                  wxString(parent_local_name.c_str(),
                                           wxConvLocal));
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
	wxASSERT(mCardMap.find(std::string(inName.mb_str())) == mCardMap.end());

	// Insert the card into our tree.
	std::string local_name;
	wxTreeItemId parent_id =
        FindParentContainer(std::string(inName.mb_str()), local_name);
	wxTreeItemId id =
        mTree->AppendItem(parent_id, wxString(local_name.c_str(), wxConvLocal));
	mTree->SetItemData(id, new CardItemData(mTree, inName));
	mTree->SetIcon(id, ProgramTreeCtrl::ICON_CARD,
				   ProgramTreeCtrl::ICON_CARD);

	// Record the card in our map.
	mCardMap.insert(ItemMap::value_type(std::string(inName.mb_str()), id));
}

void ProgramTree::SetDefaultWidth(int inWidth)
{
	SetDefaultSize(wxSize(inWidth, 0 /* unused */));
}

void ProgramTree::NotifyReloadScriptStarting()
{
    ASSERT(mCardsID.IsOk());
	mCardMap.clear();
	mSequenceMap.clear();
	mTree->CollapseAndReset(mCardsID);
}


void ProgramTree::NotifyEnterCard(const wxString &inName)
{
	// Look up the ID corresponding to this card.
	ItemMap::iterator found = mCardMap.find(std::string(inName.mb_str()));
	wxASSERT(found != mCardMap.end());

	// Move the highlighting to the appropriate card.
	if (mHaveLastHighlightedItem)
		mTree->SetItemBold(mLastHighlightedItem, FALSE);
	mTree->SetItemBold(found->second);
	mHaveLastHighlightedItem = true;
	mLastHighlightedItem = found->second;
	mTree->EnsureVisible(found->second);
}
