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

#ifndef Element_H
#define Element_H

#include "Node.h"

class wxAccessible;
class DrawingArea;
class CairoContext;


/// A Element is something which appears on a stage--typically either
/// a Zone or an full-fledged widget.  Elements are either "lightweight",
/// meaning they need help from the stage to process events, or they are
/// heavyweight and use wxWindows to process their events.  Zones are
/// lightweight; widgets are heavyweight.
///
/// This class has a "fat" interface--a number of methods are only useful
/// if IsLightWeight returns true.  This allows us to avoid using RTTI,
/// but is otherwise a slightly odd design.
class Element : public Node {
    bool mHasLegacyZOrderAndVisibility;

public:
    /// Create a new Element and attach it to inStage.  The stage is
    /// responsible for deleting the element.
    Element(const wxString &inName,
            Halyard::TCallbackPtr inDispatcher = Halyard::TCallbackPtr());
    virtual ~Element() {}

    virtual Type GetType() { return ELEMENT; }
    virtual void Register();
    virtual void Unregister();
    
    /// Ideally, elements should be treated as an inseperable part of their
    /// parent node.  But historically, elements were treated as immediate
    /// children of the current card for two purposes: Z-order, and
    /// hide/show support.  We need to maintain compatibility with these
    /// legacy semantics on a per-element basis, because doing anything
    /// else would break quite a bit of existing Scheme code.
    ///
    /// \see UseLegacyZOrderAndVisibility()
    virtual bool HasLegacyZOrderAndVisibility() const
        { return mHasLegacyZOrderAndVisibility; }

    /// Tell this element to use backwards-compatible Z-order and
    /// visibility semantics.
    ///
    /// \see HasLegacyZOrderAndVisibility()
    virtual void UseLegacyZOrderAndVisibility();

    /// Move the element to the specified location.
    virtual void MoveTo(const wxPoint &inPoint);
    
    /// Get an appropriate cursor for this object.  We use the cursor name,
    /// not the actual Cursor, so that we don't hold onto an illegal
    /// CursorPtr reference.  See the Cursor documentation for details.
    virtual std::string GetCursorName() { return "hand"; }

    /// Return the DrawingArea associated with this element, if any.
    virtual DrawingArea *GetDrawingArea() { return NULL; }

    /// Get the accessibility information for this element, or NULL, if
    /// it has no accessibility information.
    virtual wxAccessible *GetAccessible() { return NULL; }
};

#endif // Element_H
