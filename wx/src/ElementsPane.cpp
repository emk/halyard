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

#include "AppHeaders.h"
#include "StageFrame.h"
#include "ElementsPane.h"
#include "Node.h"

using namespace Halyard;

BEGIN_EVENT_TABLE(ElementsPane, wxTreeCtrl)
END_EVENT_TABLE()

ElementsPane::ElementsPane(StageFrame *inStageFrame)
    : CustomTreeCtrl(inStageFrame, wxID_ANY, wxDefaultPosition, wxSize(0, 0))
{
}

void ElementsPane::RegisterNode(NodePtr inNode) {
    // Create a new item in our tree.
    ASSERT(mItemMap.find(inNode->GetName()) == mItemMap.end());
    wxTreeItemId item;
    bool is_anonymous;
    if (inNode->IsRootNode()) {
        item = AddRoot(inNode->GetDisplayName(&is_anonymous));
    } else {
        ItemMap::iterator found_parent =
            mItemMap.find(inNode->GetParent()->GetName());
        ASSERT(found_parent != mItemMap.end());
        item = AppendItem(found_parent->second,
                          inNode->GetDisplayName(&is_anonymous));

        // If our parent is not an Element, expand it so we can see this
        // node.
        if (inNode->GetParent()->GetType() != Node::ELEMENT)
            Expand(found_parent->second);
    }

    // If the node is anonymous, show the placeholder name in italics.
    if (is_anonymous) {
        wxFont item_font(GetItemFont(item));
        item_font.SetStyle(wxFONTSTYLE_ITALIC);
        SetItemFont(item, item_font);
    }

    // Add an appropriate icon.
    switch (inNode->GetType()) {
        case Node::CARD_GROUP:
            SetIcon(item, ICON_FOLDER_CLOSED, ICON_FOLDER_OPEN);
            break;
        case Node::CARD:
            SetIcon(item, ICON_CARD);
            break;
        case Node::ELEMENT:
            SetIcon(item, ICON_ELEMENT);
            break;
    }

    // Set up any properties which depend on the dynamic state of the node.
    UpdateItemForDynamicNodeState(item, inNode);

    // Record the new item in mItemMap, so we can look it up by name.
    mItemMap.insert(ItemMap::value_type(inNode->GetName(), item));
}

void ElementsPane::UnregisterNode(NodePtr inNode) {
    ItemMap::iterator found(mItemMap.find(inNode->GetName()));
    ASSERT(found != mItemMap.end());
    Delete(found->second);
    mItemMap.erase(found);
}

void ElementsPane::NotifyNodeStateChanged(NodePtr inNode) {
    ItemMap::iterator found(mItemMap.find(inNode->GetName()));
    ASSERT(found != mItemMap.end());
    UpdateItemForDynamicNodeState(found->second, inNode);
}

void ElementsPane::UpdateItemForDynamicNodeState(wxTreeItemId inItem,
                                                 NodePtr inNode)
{
    // Show visible nodes in black, and hidden nodes in grey.
    if (inNode->IsShown())
        SetItemTextColour(inItem, *wxBLACK);
    else
        SetItemTextColour(inItem, wxColour(0x60, 0x60, 0x60));
}
