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
#include "LightweightElement.h"
#include "EventDispatcher.h"
#include "Stage.h"

LightweightElement::LightweightElement(Stage *inStage, const wxString &inName,
									   FIVEL_NS TCallbackPtr inDispatch,
									   wxCursor &inCursor)
    : Element(inStage, inName, inDispatch), mCursor(inCursor), mIsShown(true),
      mWantsCursor(false)
{
}

void LightweightElement::Show(bool inShow) {
    if (inShow != mIsShown) {
        // Update our state, and notify the stage.
        mIsShown = inShow;
        GetStage()->NotifyElementsChanged();
    }
}
