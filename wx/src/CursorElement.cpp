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
#include "CursorElement.h"
#include "CursorManager.h"
#include "HalyardApp.h"
#include "Stage.h"
#include "EventDispatcher.h"

using namespace Halyard;

CursorElement::CursorElement(Stage *inStage, const wxString &inName,
                             const wxRect &inBounds,
                             Halyard::TCallbackPtr inDispatch,
                             bool inHasAlpha,
                             const std::string &inCursorRegName)
    : Overlay(inStage, inName, inBounds, inDispatch, "blank", inHasAlpha),
      mIsRegistered(false), mCursorRegName(inCursorRegName)
{
}

CursorElement::~CursorElement() {
    ASSERT(!mIsRegistered);
}

void CursorElement::RegisterWithCursorManager() {
    ASSERT(!mIsRegistered);
    // Register this element with the CursorManager.
    CursorManager *manager = wxGetApp().GetStage()->GetCursorManager();
    manager->RegisterElementCursor(mCursorRegName, this);
    mIsRegistered = true;
    // TODO - We may need to add a Stage::NotifyElementsChanged() call
    // here (or in the cursor manager), because we really should be
    // updating the current cursor as soon as we successfully register
    // a cursor.  It seems to work as is, but if you run into a bug in
    // which newly created cursors are not used soon enough, you may
    // need to change this.
}

void CursorElement::UnregisterWithCursorManager(CursorManager *inManager) {
    ASSERT(mIsRegistered);
    inManager->UnregisterElementCursor(mCursorRegName, this);
    mIsRegistered = false;
}

void CursorElement::SetStageCursor(const wxPoint &point) {
    ASSERT(mIsRegistered);
    SetStageCursorTo(wxCursor(wxCURSOR_BLANK));
    GetEventDispatcher()->DoEventCursorMoved(point);
    GetEventDispatcher()->DoEventCursorShown();
}

void CursorElement::MoveCursor(const wxPoint &point) {
    ASSERT(mIsRegistered);
    GetEventDispatcher()->DoEventCursorMoved(point);
	wxGetApp().GetStage()->RefreshStage("none", 0);
}

void CursorElement::UnsetStageCursor() {
    ASSERT(mIsRegistered);
    GetEventDispatcher()->DoEventCursorHidden();
}
