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
#include "CommonWxConv.h"
#include "CustomTreeCtrl.h"
#include "ProgramTree.h"
#include "Model.h"
#include "ModelView.h"
#include "doc/Document.h"
#include "doc/HalyardProgram.h"
#include "dlg/ProgramPropDlg.h"

using namespace Halyard;

class ProgramTreeItemData;
class NodeItemData;


//=========================================================================
//  ProgramTreeCtrl
//=========================================================================

/// Tree widget showing structure of Halyard script.
class ProgramTreeCtrl : public CustomTreeCtrl
{
public:
	ProgramTreeCtrl(wxWindow *inParent);
    NodeItemData *GetNodeItemData(wxTreeItemId inId);
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
//  NodeItemData
//=========================================================================

/// For now, a node is either a card or a group.
class NodeItemData : public CustomTreeItemData {
    wxString mName;
    bool mIsPlaceHolder;

public:
    NodeItemData(ProgramTreeCtrl *inTreeCtrl, const wxString &inName,
                 bool inIsPlaceHolder);

    /// Is this node a card?
    virtual bool IsCard() const { return false; }

    /// The full name of this node.
    wxString GetName() const { return mName; }

    /// Is this node a placeholder?
    bool IsPlaceHolder() const { return mIsPlaceHolder; }
};

NodeItemData::NodeItemData(ProgramTreeCtrl *inTreeCtrl, const wxString &inName,
                           bool inIsPlaceHolder)
    : CustomTreeItemData(inTreeCtrl), mName(inName),
      mIsPlaceHolder(inIsPlaceHolder)
{
}


//=========================================================================
//  GroupItemData
//=========================================================================

/// Sequences of cards can be nested within each other.
class GroupItemData : public NodeItemData {
public:
	GroupItemData(ProgramTreeCtrl *inTreeCtrl, const wxString &inName,
                  bool inIsPlaceHolder);
};

GroupItemData::GroupItemData(ProgramTreeCtrl *inTreeCtrl,
                             const wxString &inName,
                             bool inIsPlaceHolder)
	: NodeItemData(inTreeCtrl, inName, inIsPlaceHolder)
{
}


//=========================================================================
//  CardItemData
//=========================================================================

/// Representation of a card in our ProgramTreeCtrl.
class CardItemData : public NodeItemData {
public:
	CardItemData(ProgramTreeCtrl *inTreeCtrl, wxString inName,
                 bool inIsPlaceHolder);
    virtual bool IsCard() const { return true; }
	virtual void OnLeftDClick(wxMouseEvent& event);
};

CardItemData::CardItemData(ProgramTreeCtrl *inTreeCtrl, wxString inName,
                           bool inIsPlaceHolder)
	: NodeItemData(inTreeCtrl, inName, inIsPlaceHolder)
{
}

void CardItemData::OnLeftDClick(wxMouseEvent& event)
{
	Stage *stage = wxGetApp().GetStage();
	if (stage->CanJump())
		stage->TryJumpTo(GetName());
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

/// Look up a the NodeItemData object associated with a particular tree
/// node.  It is an error to call this on a node which doesn't have an
/// associated node.
NodeItemData *ProgramTreeCtrl::GetNodeItemData(wxTreeItemId inItemId)  {
    NodeItemData *data = dynamic_cast<NodeItemData*>(GetItemData(inItemId));
    ASSERT(data);
    return data;
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

bool ProgramTree::IsCardItem(wxTreeItemId inItemId) {
    return mTree->GetNodeItemData(inItemId)->IsCard();
}

bool ProgramTree::IsPlaceHolderItem(wxTreeItemId inItemId) {
    return mTree->GetNodeItemData(inItemId)->IsPlaceHolder();
}

void ProgramTree::AnalyzeNodeName(const std::string &inName,
                                  bool &outIsRootNode,
                                  std::string &outParentName,
                                  std::string &outLocalName)
{
    // Look backwards through the string for slashes, to separate the
    // node name from the parent name.
    std::string::size_type slashpos = inName.rfind('/');
    if (slashpos == 0) {
        // The only slash is at the start of the string, so we're either
        // the root node, or one of its immediate children.
        if (inName == "/") {
            outIsRootNode = true;
            outParentName = "";
            outLocalName  = "";
        } else {
            outIsRootNode = false;
            outParentName = "/";
            outLocalName  = std::string(inName, 1, std::string::npos);
        }
    } else if (slashpos == std::string::npos || slashpos == inName.size() - 1) {
		// Either we have no slash, or we have a slash at the end of the
        // name.  Either way, we don't like it.
		gLog.FatalError("Illegal card name: \"%s\"", inName.c_str());
    } else {
        outIsRootNode = false;
        outParentName = std::string(inName, 0, slashpos);
		outLocalName  = std::string(inName, slashpos + 1, std::string::npos);

        // Make sure that the newly-split parent name is reasonable.  This
        // is necessary if we're going to detect malformed names like
        // "//foo".
        ASSERT(outParentName.size() > 0);
        if (outParentName[outParentName.size()-1] == '/')
            gLog.FatalError("Illegal card name: \"%s\"", inName.c_str());
    }
}

wxTreeItemId ProgramTree::FindOrCreateGroupMember(const std::string &inName,
                                                  bool inIsCard,
                                                  bool inIsPlaceHolder)
{
    ASSERT(mCardsID.IsOk());

    wxTreeItemId result;
    ItemMap::iterator found = mGroupMemberMap.find(inName);
    if (found != mGroupMemberMap.end()) {
        // We already have this name in mGroupMemberMap.
        result = found->second;
    } else {
        // Split our node name into a parent component and a local
        // component.  We should never encounter the root node on this
        // branch, because it is preloaded into mGroupMemberMap.
        bool is_root_node;
        std::string parent_name, local_name;
        AnalyzeNodeName(inName, is_root_node, parent_name, local_name);
        ASSERT(!is_root_node);

        // Look up our parent node.  This should never be a placeholder,
        // because otherwise, how could it have children?
        wxTreeItemId parent_id =
            FindOrCreateGroupMember(parent_name, false, false);

        // Create a new node of the appropriate type.
        wxString name_wx(ToWxString(inName));
        wxString local_name_wx(ToWxString(local_name));
        result = mTree->AppendItem(parent_id, local_name_wx);
        if (inIsCard) {
            mTree->SetItemData(result, new CardItemData(mTree, name_wx,
                                                        inIsPlaceHolder));
            mTree->SetIcon(result, ProgramTreeCtrl::ICON_CARD,
                           ProgramTreeCtrl::ICON_CARD);
            
        } else {
			mTree->SetItemData(result, new GroupItemData(mTree, name_wx,
                                                         inIsPlaceHolder));
			mTree->SetIcon(result, ProgramTreeCtrl::ICON_FOLDER_CLOSED,
						   ProgramTreeCtrl::ICON_FOLDER_OPEN);
            
        }

        // Register our node in mGroupMemberMap for future look-ups.
        mGroupMemberMap.insert(ItemMap::value_type(inName, result));
    }

    // We don't actually have a NodeItemData object on the root node, at
    // least for now.
    ASSERT(inName == "/" || IsCardItem(result) == inIsCard);
    ASSERT(inName == "/" || IsPlaceHolderItem(result) == inIsPlaceHolder);
    return result;
}

void ProgramTree::RegisterGroupMember(const wxString &inName, bool inIsCard,
                                      bool inIsPlaceHolder)
{
    (void) FindOrCreateGroupMember(ToStdString(inName), inIsCard,
                                   inIsPlaceHolder);
}

void ProgramTree::SetDefaultWidth(int inWidth)
{
	SetDefaultSize(wxSize(inWidth, 0 /* unused */));
}

void ProgramTree::NotifyReloadScriptStarting()
{
    ASSERT(mCardsID.IsOk());
	mGroupMemberMap.clear();
    mHaveLastHighlightedItem = false;
	mTree->CollapseAndReset(mCardsID);

    // Register our root node in mGroupMemberMap so that FindGroupMember
    // is guaranteed to have a recursive base case.
    mGroupMemberMap.insert(ItemMap::value_type("/", mCardsID));
}

void ProgramTree::NotifyEnterCard(const wxString &inName)
{
	// Look up the ID corresponding to this card.
	ItemMap::iterator found =
        mGroupMemberMap.find(std::string(inName.mb_str()));
	wxASSERT(found != mGroupMemberMap.end());
    ASSERT(IsCardItem(found->second));

	// Move the highlighting to the appropriate card.
	if (mHaveLastHighlightedItem)
		mTree->SetItemBold(mLastHighlightedItem, FALSE);
	mTree->SetItemBold(found->second);
	mHaveLastHighlightedItem = true;
	mLastHighlightedItem = found->second;
	mTree->EnsureVisible(found->second);
}
