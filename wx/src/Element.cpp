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

#include <boost/foreach.hpp>

#include "Element.h"
#include "Card.h"
#include "HalyardApp.h"
#include "Stage.h"

using namespace Halyard;


//=========================================================================
//  Element Methods
//=========================================================================

Element::Element(const wxString &inName, Halyard::TCallbackPtr inDispatcher)
    : Node(inName, inDispatcher), mHasLegacyZOrderAndVisibility(false)
{
}

NodePtr Element::GetParentForPurposeOfZOrderAndVisibility() {
    if (HasLegacyZOrderAndVisibility()) {
        CardPtr card(wxGetApp().GetStage()->GetCurrentCard());
        if (card)
            return card;

        // We don't have a reasonable return value when there isn't a
        // current card, so just fall through.
        gLog.Warn("halyard.node",
                  "Called GetParentForPurposeOfZOrderAndVisibility on %s "
                  "when there isn't a current card.", GetLogName());
    }
    return GetParent();
}

void Element::UseLegacyZOrderAndVisibility() {
    ASSERT(!mHasLegacyZOrderAndVisibility);
    mHasLegacyZOrderAndVisibility = true;
    ElementPtr as_shared(shared_from_this(), dynamic_cast_tag());
    ASSERT(as_shared);
    wxGetApp().GetStage()->RegisterLegacyZOrderAndVisibility(as_shared);
}

void Element::MoveTo(const wxPoint &inPoint) {
    OperationNotSupported("move");
}

void Element::Register() {
    ElementPtr as_shared(shared_from_this(), dynamic_cast_tag());
    ASSERT(as_shared);
    GetParent()->RegisterChildElement(as_shared);
    Node::Register(); // Do this after our parent knows about us.
}

void Element::Unregister() {
    Node::Unregister(); // Do this while our parent still knows about us.
    ElementPtr as_shared(shared_from_this(), dynamic_cast_tag());
    ASSERT(as_shared);
    GetParent()->UnregisterChildElement(as_shared);
    if (HasLegacyZOrderAndVisibility())
        wxGetApp().GetStage()->UnregisterLegacyZOrderAndVisibility(as_shared);
}

void Element::RecursivelyInvalidateCompositing() {
    InvalidateCompositing();
    BOOST_FOREACH(ElementPtr elem, GetElements())
        if (IsChildForPurposeOfZOrderAndVisibility(elem))
            elem->RecursivelyInvalidateCompositing();
}
