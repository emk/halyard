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

#ifndef CursorElement_H
#define CursorElement_H

#include "Overlay.h"
#include "Cursor.h"

class CursorManager;

/// A overlay is the simplest form of lightweight element.
class CursorElement : public Overlay, public FIVEL_NS Cursor
{
    std::string mCursorRegName;

public:
    CursorElement(Stage *inStage, const wxString &inName,
                  const wxRect &inBounds, FIVEL_NS TCallbackPtr inDispatch,
                  bool inHasAlpha,
                  const std::string &inCursorRegistrationName);
    virtual ~CursorElement();

    /// Register this element with the CursorManager.
    void Register(CursorManager *inManager,
                  shared_ptr<CursorElement> inSharedPtr);

    /// Unregister this element with the CursorManager.
    void Unregister(CursorManager *inManager);

    virtual void SetStageCursor(const wxPoint &point);
    virtual void MoveCursor(const wxPoint &point);
    virtual void UnsetStageCursor();
};

#endif // CursorElement_H
