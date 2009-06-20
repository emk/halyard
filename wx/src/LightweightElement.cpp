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

#include <wx/access.h>

#include "LightweightElement.h"
#include "LightweightAccessible.h"
#include "EventDispatcher.h"
#include "Stage.h"
#include "HalyardApp.h"

using namespace Halyard;


LightweightElement::LightweightElement(const wxString &inName,
                                       Halyard::TCallbackPtr inDispatch,
                                       const std::string &inCursorName)
    : Element(inName, inDispatch), mCursorName(inCursorName),
      mIsShown(true), mWantsCursor(false), mIsInDragLayer(false)
{
#if wxUSE_ACCESSIBILITY
    mAccessible = shared_ptr<wxAccessible>(new LightweightAccessible(this));
#endif // wxUSE_ACCESSIBILITY
}

std::string LightweightElement::GetCursorName() {
    // If we have our own cursor, return it.
    if (mCursorName == "inherit")
        return GetParent()->GetCursorName();
    else
        return mCursorName;
}

void LightweightElement::SetCursorName(const std::string &inCursorName) {
    mCursorName = inCursorName; 
    // We need to notify the stage that elements have changed so it can
    // update our current cursor.
    wxGetApp().GetStage()->NotifyNodesChanged();
}

bool LightweightElement::IsInDragLayer() const {
    return mIsInDragLayer;
}

void LightweightElement::SetInDragLayer(bool inDragLayer) {
    mIsInDragLayer = inDragLayer;
}
