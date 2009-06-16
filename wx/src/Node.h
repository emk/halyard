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

/// A Node represents something in the "tree" of the program: a card, a group
/// or an element.
class Node : boost::noncopyable, public boost::enable_shared_from_this<Node> {
    /// A list of elements.
    typedef std::deque<ElementPtr> ElementList;

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

protected:
    /// Throw an error saying inOperationName is not allowed.
    void OperationNotSupported(const char *inOperationName);

public:
    /// A very high-level breakdown of different Node types.
    enum Type {
        CARD_GROUP,
        CARD,
        ELEMENT
    };

    /// Create a new Node.  The stage is responsible for deleting the node.
    Node(const wxString &inName,
         Halyard::TCallbackPtr inDispatcher = Halyard::TCallbackPtr());
    virtual ~Node() {}

    /// Is this the root of our node hierarchy?
    bool IsRootNode() { return (mParent == NULL); }

    /// Get the parent of this node.  Should only be called if IsRootNode()
    /// is false.
    NodePtr GetParent() { ASSERT(mParent); return mParent; }

    /// Return the name of the node.  Should be unique.
    wxString GetName() { return mName; }

    /// Return the name of the node, omitting the parent's portion of the
    /// name.
    wxString GetDisplayName();

    /// Return the name of this node in a fashion suitable for logging.
    /// The 'const char *' returned is guaranteed to remain valid until the
    /// node is destroyed.
    const char *GetLogName() { return mLogName.c_str(); }

    /// What type of node is this?  This predicate allows other parts of
    /// the engine to distinguish between a few high-level types of Node
    /// without adding new virtual functions to this class or using dynamic
    /// casts.
    virtual Type GetType() = 0;

    /// Get the event dispatcher associated with this node.
    EventDispatcherPtr GetEventDispatcher() {
        ASSERT(mEventDispatcher.get());
        return mEventDispatcher;
    }

    /// Return true if the node can be shown.
    virtual bool HasVisibleRepresentation() { return true; }

    /// Let the node do any idle-time processing it needs to do.
    virtual void Idle() {}

    /// Return true if the node is shown on the screen.
    virtual bool IsShown() { return true; }

    /// Show or hide the widget.
    virtual void Show(bool inShow);

    /// Does this node want the engine to display a cursor?
    virtual bool WantsCursor() const { return false; }

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

    /// Register this node with its parent, and with other objects (except
    /// the Stage).  This is not part of the constructor because it needs
    /// to call virtual functions (and because we may or may not want to
    /// control exactly when nodes get registered).
    virtual void Register();

    /// Unregister this node from its parent and any other objects (except
    /// the Stage).
    virtual void Unregister();

    /// Attach a child Element to this node.
    void RegisterChildElement(ElementPtr inElem);

    /// Detach a child Element from this node.
    void UnregisterChildElement(ElementPtr inElem);
};

#endif // Node_H
