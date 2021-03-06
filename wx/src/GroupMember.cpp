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
#include "GroupMember.h"
#include "CardGroup.h"
#include "HalyardApp.h"
#include "Stage.h"

using namespace Halyard;


//=========================================================================
//  GroupMember Methods
//=========================================================================

GroupMember::GroupMember(const wxString &inName,
                         Halyard::TCallbackPtr inDispatcher)
    : Node(inName, inDispatcher)
{
}

void GroupMember::Register() {
    if (!IsRootNode()) {
        GroupMemberPtr as_shared(shared_from_this(), dynamic_cast_tag());
        ASSERT(as_shared);
        CardGroupPtr parent(GetParent(), dynamic_cast_tag());
        ASSERT(parent);
        parent->RegisterMember(as_shared);
    }
    Node::Register(); // Do this after our parent knows about us.
}
 
void GroupMember::Unregister() {
    Node::Unregister(); // Do this while our parent still knows about us.
    if (!IsRootNode()) {
        GroupMemberPtr as_shared(shared_from_this(), dynamic_cast_tag());
        ASSERT(as_shared);
        CardGroupPtr parent(GetParent(), dynamic_cast_tag());
        ASSERT(parent);
        parent->UnregisterMember(as_shared);
    }
}

bool GroupMember::IsChildForPurposeOfZOrderAndVisibility(ElementPtr inElem) {
    // We draw all members of mElements, regardless of the value of
    // HasLegacyZOrderAndVisibility, if we are the current group
    // member.  Otherwise, we only draw our real, non legacy
    // children, as the current group member should be drawing
    // our legacy children.
    if (wxGetApp().GetStage()->GetCurrentGroupMember().get() == this) {
        return true;
    } else { 
        return Node::IsChildForPurposeOfZOrderAndVisibility(inElem);
    }
}
