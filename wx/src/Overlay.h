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

#ifndef Overlay_H
#define Overlay_H

#include "LightweightElement.h"
#include "DrawingArea.h"


/// A overlay is the simplest form of lightweight element.
class Overlay : public LightweightElement {
    DrawingArea mDrawingArea;
    bool mAreTransparentAreasClickable;
    
public:
    Overlay(const wxString &inName, const wxRect &inBounds,
            Halyard::TCallbackPtr inDispatch, const std::string &inCursorName,
            bool inHasAlpha, bool inAreTransparentAreasClickable = false);

    virtual void Show(bool inShow);
    virtual bool IsPointInElement(const wxPoint &inPoint);
    virtual void MoveTo(const wxPoint &inPoint);
    virtual void DrawBorder(wxDC &inDC);
    virtual DrawingArea *GetDrawingArea() { return &mDrawingArea; }
    virtual void SetInDragLayer(bool inDragLayer);
    virtual void CompositeInto(CairoContext &inCr);

    /// Set the size of this overlay.  Erases all contents.
    void SetSize(const wxSize &inSize);
};

#endif // Overlay_H
