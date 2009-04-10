// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

#include "EventDispatcher.h"

class wxAccessible;
class Stage;
class DrawingArea;


//////////
/// A Element is something which appears on a stage--typically either
/// a Zone or an full-fledged widget.  Elements are either "lightweight",
/// meaning they need help from the stage to process events, or they are
/// heavyweight and use wxWindows to process their events.  Zones are
/// lightweight; widgets are heavyweight.
///
/// This class has a "fat" interface--a number of methods are only useful
/// if IsLightWeight returns true.  This allows us to avoid using RTTI,
/// but is otherwise a slightly odd design.
///
class Element
{
	//////////
	/// The stage on which this element appears.
	///
	Stage *mStage;

	//////////
	/// The name of this element.  Must be unique on any given card. 
	///
	wxString mName;

    //////////
    /// Because there's no way to extract a 'const char *' from a wxString
    /// without it disappearing almost immediately, we also keep a
    /// std::string version of mName for convenient use with logging
    /// functions.
    ///
    std::string mLogName;

    //////////
    /// The event dispatcher for this object.
    ///
    EventDispatcherPtr mEventDispatcher;

protected:
    //////////
    /// Throw an error saying inOperationName is not allowed.
    ///
    void OperationNotSupported(const char *inOperationName);

public:
	//////////
	/// Create a new Element and attach it to the specified stage.
	/// The stage is responsible for deleting the element.
	///
	Element(Stage *inStage, const wxString &inName,
            Halyard::TCallbackPtr inDispatcher = Halyard::TCallbackPtr());

	virtual ~Element() {}
	
	//////////
	/// Return the stage on which the element appears.
	///
	Stage *GetStage() { return mStage; }

	//////////
	/// Return the name of the element.  Should be unique on any
	/// given card.
	///
	wxString GetName() { return mName; }

    //////////
    /// Return the name of this element in a fashion suitable for logging.
    /// The 'const char *' returned is guaranteed to remain valid until the
    /// element is destroyed.
    ///
    const char *GetLogName() { return mLogName.c_str(); }

    //////////
    /// Get the event dispatcher associated with this element.
    ///
    EventDispatcherPtr GetEventDispatcher() {
        ASSERT(mEventDispatcher.get());
        return mEventDispatcher;
    }

	//////////
	/// Return true if the element can be shown.
	///
	virtual bool HasVisibleRepresentation() { return true; }

    //////////
    /// Let the element do any idle-time processing it needs to do.
    ///
    virtual void Idle() {}

	//////////
	/// Return true if the element is shown on the screen.
	///
	virtual bool IsShown() { return true; }

	//////////
	/// Show or hide the widget.
	///
	virtual void Show(bool inShow);

    //////////
    /// Does this element want the engine to display a cursor?
    ///
    virtual bool WantsCursor() const { return false; }

	//////////
	/// Does this element need to receive events from the Stage?
	///
	virtual bool IsLightWeight() { return false; }

	//////////
	/// Is the specified point in the element?  If this function ever returns
	/// true, then GetEventDispatcher must not return NULL.
	/// NOT USEFUL UNLESS IsLightWeight RETURNS TRUE.
	///
	virtual bool IsPointInElement(const wxPoint &inPoint) { return false; }

    //////////
    /// Move the element to the specified location.
    ///
    virtual void MoveTo(const wxPoint &inPoint);
	
	//////////
	/// Get an appropriate cursor for this object.  We use the cursor name,
    /// not the actual Cursor, so that we don't hold onto an illegal
    /// CursorPtr reference.  See the Cursor documentation for details.
	///
	virtual std::string GetCursorName() { return "hand"; }

	/////////
	/// Draw the element to the specified DC
	///
	virtual void DrawElementBorder(wxDC &inDC) {}

	//////////
	/// Return the DrawingArea associated with this element, if any.
	///
	virtual DrawingArea *GetDrawingArea() { return NULL; }

    //////////
    /// Certain elements can be temporarily raised into the "drag layer",
    /// which is above other LightweightElements (e.g., zones and
    /// overlays) but below Widgets (e.g., QuickTime movies).
    ///
    virtual bool IsInDragLayer() const { return false; }

	//////////
	/// Composite our data into the specified DC.
	///
	/// \param inDC  The compositing DC.
	/// \param inClipRect  The rectangle (in terms of inDC co-ordinates)
	///                   which we're updating.
	///
	virtual void CompositeInto(wxDC &inDC, const wxRect &inClipRect) {}

    //////////
    /// When we redraw the Stage, we want to exclude certain elements
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

    //////////
    /// Get the accessibility information for this element, or NULL, if
    /// it has no accessibility information.
    ///
    virtual wxAccessible *GetAccessible() { return NULL; }
};

#endif // Element_H
