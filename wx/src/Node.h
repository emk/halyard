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

#ifndef Node_H
#define Node_H

#include "EventDispatcher.h"

class Node;
typedef shared_ptr<Node> NodePtr;
class Element;
typedef shared_ptr<Element> ElementPtr;
class ElementsPane;
class CairoContext;

/// A Node represents something in the "tree" of the program: a card, a group
/// or an element.
class Node : boost::noncopyable, public boost::enable_shared_from_this<Node> {
    /// A list of elements.
    typedef std::deque<ElementPtr> ElementList;

    /// How many nodes currently exist?
    static int sNodeCount;

    /// The parent of this node, or NULL if this is the root node.
    NodePtr mParent;

    /// The name of this node.  Must be unique on any given card. 
    wxString mName;

    /// Because there's no way to extract a 'const char *' from a wxString
    /// without it disappearing almost immediately, we also keep a
    /// std::string version of mName for convenient use with logging
    /// functions.
    std::string mLogName;

    /// The event dispatcher for this object.
    EventDispatcherPtr mEventDispatcher;

    /// The elements attached to this node.
    ElementList mElements;

public:
    /// Create a new Node.  The stage is responsible for deleting the node.
    Node(const wxString &inName,
         Halyard::TCallbackPtr inDispatcher = Halyard::TCallbackPtr());
    virtual ~Node();

    /// Return the number of nodes that currently exist.
    static int NodeCount() { return sNodeCount; }

    ///////////////////////////////////////////////////////////////////////
    /// \name Node type
    //@{

    /// A very high-level breakdown of different Node types.
    enum Type {
        CARD_GROUP,
        CARD,
        ELEMENT
    };

    /// What type of node is this?  This predicate allows other parts of
    /// the engine to distinguish between a few high-level types of Node
    /// without adding new virtual functions to this class or using dynamic
    /// casts.
    virtual Type GetType() = 0;

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Name
    //@{

    /// Return the name of the node.  Should be unique.
    wxString GetName() { return mName; }

    /// Return the name of the node, omitting the parent's portion of the
    /// name.
    ///
    /// \param outIsAnonymous If this pointer is supplied, the Boolean
    ///     variable will be set to true if the Node should treated as
    ///     anonymous, and false otherwise.
    wxString GetDisplayName(bool *outIsAnonymous = NULL);

    /// Return the name of this node in a fashion suitable for logging.
    /// The 'const char *' returned is guaranteed to remain valid until the
    /// node is destroyed.
    const char *GetLogName() { return mLogName.c_str(); }

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Parent and child relationships
    //@{

protected:
    /// Return mElements so subclasses can iterate over it.
    ElementList &GetElements() { return mElements; }

    /// Is inElem a real child of this node?
    ///
    /// \see IsChildForPurposeOfZOrderAndVisibility()
    bool IsRealChild(ElementPtr inElem);

    /// Should we treat inElem as a child of this node when calculating
    /// Z-order and element visibility?  This is a backwards-compatible
    /// emulation mode needed to run old code.  Precondition: inElem is a
    /// member of inElements.
    ///
    /// \see IsRealChild()
    /// \see Element::HasLegacyZOrderAndVisibility()
    virtual bool IsChildForPurposeOfZOrderAndVisibility(ElementPtr inElem);

public:
    /// Is this the root of our node hierarchy?
    bool IsRootNode() { return (mParent == NULL); }

    /// Get the parent of this node.  Should only be called if IsRootNode()
    /// is false.
    NodePtr GetParent() { ASSERT(mParent); return mParent; }

    /// Like GetParent(), but returns the current card if
    /// Element::HasLegacyZOrderAndVisibility() is true.
    virtual NodePtr GetParentForPurposeOfZOrderAndVisibility();

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Visibility
    //@{

protected:
    /// This function will be called by Show() when a hidden node needs to
    /// be shown or hidden.
    virtual void DoShow(bool inShow);

public:
    /// Return true if the node can be shown.
    virtual bool HasVisibleRepresentation() { return true; }

    /// Return true if the node is shown on the screen.
    virtual bool IsShown() { return true; }

    /// Show or hide the Node.  This function cannot be overridden, though
    /// it does call functions which can.
    ///
    /// \see DoShow()
    void Show(bool inShow);

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Events
    //@{

    /// Get the event dispatcher associated with this node.
    EventDispatcherPtr GetEventDispatcher() {
        ASSERT(mEventDispatcher.get());
        return mEventDispatcher;
    }

    /// Should we receive events when inNode is grabbed?  Returns true if
    /// and only if this node is inNode, or we can reach inNode by
    /// repeatedly calling GetParentForPurposeOfZOrderAndVisibility().
    bool ShouldReceiveEventsWhenGrabbing(NodePtr inNode);

    /// Let the node do any idle-time processing it needs to do.
    virtual void Idle() {}

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Cursors
    //@{

    /// Does this node want the engine to display a cursor?
    virtual bool WantsCursor() const { return false; }

    /// Get an appropriate cursor for this object.  We use the cursor name,
    /// not the actual Cursor, so that we don't hold onto an illegal
    /// CursorPtr reference.  See the Cursor documentation for details.
    virtual std::string GetCursorName() { return "arrow"; }

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Redrawing the stage
    //@{

    /// Draw a border around the Node.
    virtual void DrawBorder(wxDC &inDC) {}

    /// When we redraw the Stage, we want to exclude certain nodes
    /// (generally Widgets) from the redraw.  This helps prevent accidental
    /// redraws *over* playing movies or native OS widgets, which is
    /// helpful, because such redraws are generally very ugly.
    ///
    /// Note that this function will only be called if IsShown returns
    /// true.
    ///
    /// \param ioRegion The current clipping region.  Use Subtract to remove
    ///    areas that shouldn't be drawn.
    /// \return Return true if clipping was applied, and false if ioRegion
    ///    was left alone.
    virtual bool ApplyClippingToStage(wxRegion &ioRegion) { return false; }

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Compositing
    //@{

    /// Certain nodes can be temporarily raised into the "drag layer",
    /// which is above other LightweightElements (e.g., zones and
    /// overlays) but below Widgets (e.g., QuickTime movies).
    virtual bool IsInDragLayer() const { return false; }

    /// Composite this node and its children into the specified
    /// CairoContext.  The Cairo clipping region will be set up correctly
    /// before this function is called.  We only composite those layers
    /// specified by inLayers.
    virtual void
    RecursivelyCompositeInto(CairoContext &inCr,
                             bool inIsCompositingDragLayer = false,
                             bool inAncestorIsInDragLayer = false);
    
    /// Composite just this node into the specified CairoContext.
    ///
    /// \see RecursivelyCompositeInto()
    virtual void CompositeInto(CairoContext &inCr) {}

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Hit-testing
    //@{

    /// Find the node located at inPoint on the screen, subject to various
    /// complications and caveats.
    ///
    /// \param inMustWantCursor  If true, ignore all nodes which don't want
    ///   cursor events.
    /// \return The node at inPoint, or NULL if no node is found.
    virtual NodePtr FindNodeAt(const wxPoint &inPoint,
                               bool inMustWantCursor = true);

    /// Is the specified point in the node?  If this function ever returns
    /// true, then GetEventDispatcher must _not_ return NULL.  Note that
    /// this function returns false for Widget and its subclasses, because
    /// they have not historically been tracked by this code.
    ///
    /// \see FindNodeAt()
    virtual bool IsPointInNode(const wxPoint &inPoint) { return false; }

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Node registration and unregistration
    //@{

    /// Register this node with its parent, and with other objects (except
    /// the Stage).  This is not part of the constructor because it needs
    /// to call virtual functions (and because we may or may not want to
    /// control exactly when nodes get registered).
    virtual void Register();

    /// Unregister this node from its parent and any other objects (except
    /// the Stage).
    virtual void Unregister();

    /// This is called when we need to rebuild the ElementsPane from
    /// scratch.
    virtual void RecursivelyReregisterWithElementsPane(ElementsPane *inPane);

    /// Attach a child Element to this node.
    void RegisterChildElement(ElementPtr inElem);

    /// Detach a child Element from this node.
    void UnregisterChildElement(ElementPtr inElem);

    //@}
    ///////////////////////////////////////////////////////////////////////
    /// \name Other member functions
    //@{

protected:
    /// Throw an error saying inOperationName is not allowed.
    void OperationNotSupported(const char *inOperationName);

public:
    /// Move a child element to the end of our inElements list.
    void RaiseToTop(ElementPtr inElem);

    //@}
};

#endif // Node_H
