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

    // These two values should not be used directly, because they will be
    // ignored if HasVisibleRepresentation() is false.
    bool mIsVisible; ///< \see IsVisible()
    bool mIsShown;   ///< \see GetIsShown()

public:
    /// Create a new Element and attach it to inStage.  The stage is
    /// responsible for deleting the element.
    Element(const wxString &inName,
            Halyard::TCallbackPtr inDispatcher = Halyard::TCallbackPtr());
    virtual ~Element() {}

    ///////////////////////////////////////////////////////////////////////
    /// \name Inherited from Node
    //@{

    virtual Type GetType() { return ELEMENT; }
    virtual NodePtr GetParentForPurposeOfZOrderAndVisibility();

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Legacy Z-order and visibility support
    //@{
    
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

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Visibility
    //@{

private:
    /// Compute the visibility of this node only.
    void CalculateVisibility();

    /// Recompute the visibility of this node and its descendants.
    void RecursivelyCalculateVisibility();

protected:
    /// Is there any way to display this element on the screen?
    virtual bool HasVisibleRepresentation() { return true; }

    /// Called after the visibility of this element changes.  Override this
    /// to show and hide associated wxWindow objects, etc.
    virtual void NotifyVisibilityChanged() {}

public:
    /// An element is visibile if it and all its ancestors return true from
    /// GetIsShown().  Ancestors are determined using the same rules as
    /// GetParentForPurposeOfZOrderAndVisibility().
    virtual bool IsVisible();

    /// Is this element shown?  Note that a "shown" element can still be
    /// invisible if one of more of its parents are not currently
    /// shown--this property is local to this particular node.
    ///
    /// This method is named GetIsShown() (and not the more succinct
    /// IsShown()), because it reads a flag local to this element, and it
    /// cannot actually tell you whether the element is truly visible.  By
    /// using a slightly awkward name, we prevent people from calling this
    /// function accidentally.
    ///
    /// \see IsVisible()
    virtual bool GetIsShown();

    /// Mark an element as shown.  This may not be sufficient to make an
    /// element visible--if any of the elements ancestors are not shown,
    /// this element will not be visible.
    ///
    /// \see GetIsShown()
    /// \see IsVisible()
    virtual void SetIsShown(bool inIsShown);

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Compositing
    //@{

protected:
    /// Invalidate just the portion of the stage covered by this element,
    /// and don't do anything about its children.
    ///
    /// \see RecursivelyInvalidateCompositing()
    virtual void InvalidateCompositing() {}

public:
    /// Recomposite this Element and any of its children.
    virtual void RecursivelyInvalidateCompositing();

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Registration and unregistration
    //@{

    virtual void Register();
    virtual void Unregister();

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Other member functions
    //@{

    /// Move the element to the specified location.
    virtual void MoveTo(const wxPoint &inPoint);
    
    /// Return the DrawingArea associated with this element, if any.
    virtual DrawingArea *GetDrawingArea() { return NULL; }

    /// Get the accessibility information for this element, or NULL, if
    /// it has no accessibility information.
    virtual wxAccessible *GetAccessible() { return NULL; }

    //@}
};

#endif // Element_H
