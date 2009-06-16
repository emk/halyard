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
    ASSERT(mItemMap.find(inNode->GetName()) == mItemMap.end());
    wxTreeItemId item;
    if (inNode->IsRootNode()) {
        item = AddRoot(inNode->GetName());
    } else {
        ItemMap::iterator found_parent =
            mItemMap.find(inNode->GetParent()->GetName());
        ASSERT(found_parent != mItemMap.end());
        item = AppendItem(found_parent->second, inNode->GetName());
    }
    mItemMap.insert(ItemMap::value_type(inNode->GetName(), item));
}

void ElementsPane::UnregisterNode(NodePtr inNode) {
    ItemMap::iterator found(mItemMap.find(inNode->GetName()));
    ASSERT(found != mItemMap.end());
    Delete(found->second);
    mItemMap.erase(found);
}
