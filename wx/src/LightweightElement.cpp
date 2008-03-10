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

#include "TamaleHeaders.h"

#include <wx/access.h>

#include "LightweightElement.h"
#include "LightweightAccessible.h"
#include "EventDispatcher.h"
#include "Stage.h"

USING_NAMESPACE_FIVEL


LightweightElement::LightweightElement(Stage *inStage, const wxString &inName,
									   FIVEL_NS TCallbackPtr inDispatch,
                                       const std::string &inCursorName)
    : Element(inStage, inName, inDispatch), mCursorName(inCursorName),
      mIsShown(true), mWantsCursor(false), mIsInDragLayer(false)
{
	mAccessible = shared_ptr<wxAccessible>(new LightweightAccessible(this));
}

void LightweightElement::Show(bool inShow) {
    if (inShow != mIsShown) {
        // Update our state, and notify the stage.
        mIsShown = inShow;
        GetStage()->NotifyElementsChanged();
    }
}

void LightweightElement::SetCursorName(const std::string &inCursorName) { 
    mCursorName = inCursorName; 
    // We need to notify the stage that elements have changed so it can
    // update our current cursor.
    GetStage()->NotifyElementsChanged();
}

bool LightweightElement::IsInDragLayer() const {
    return mIsInDragLayer;
}

void LightweightElement::SetInDragLayer(bool inDragLayer) {
    mIsInDragLayer = inDragLayer;
}
