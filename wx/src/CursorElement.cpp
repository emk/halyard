// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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
#include "CursorElement.h"
#include "CursorManager.h"
#include "FiveLApp.h"
#include "Stage.h"
#include "EventDispatcher.h"

USING_NAMESPACE_FIVEL

CursorElement::CursorElement(Stage *inStage, const wxString &inName,
                             const wxRect &inBounds,
                             FIVEL_NS TCallbackPtr inDispatch,
                             bool inHasAlpha,
                             const std::string &inCursorRegName)
    : Overlay(inStage, inName, inBounds, inDispatch, "blank", inHasAlpha),
      mCursorRegName(inCursorRegName)
{
}

CursorElement::~CursorElement() {
}

void CursorElement::Register(CursorManager *inManager,
                             shared_ptr<CursorElement> inSharedPtr)
{
    ASSERT(inSharedPtr.get() == this);
    inManager->RegisterElementCursor(mCursorRegName, inSharedPtr);
}

void CursorElement::Unregister(CursorManager *inManager) {
    // We need to do this if we ever want our destructor to be called.
    inManager->UnregisterElementCursor(mCursorRegName, this);
}

void CursorElement::SetStageCursor(const wxPoint &point) {
    SetStageCursorTo(wxCursor(wxCURSOR_BLANK));
    GetEventDispatcher()->DoEventCursorMoved(point);
    GetEventDispatcher()->DoEventCursorShown();
}

void CursorElement::MoveCursor(const wxPoint &point) {
    GetEventDispatcher()->DoEventCursorMoved(point);
	wxGetApp().GetStage()->RefreshStage("none", 0);
}

void CursorElement::UnsetStageCursor() {
    GetEventDispatcher()->DoEventCursorHidden();
}
