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

#ifndef Widget_H
#define Widget_H

#include "Stage.h"
#include "Element.h"

//////////
/// A widget represents a full-fledged wxWindow object hanging around on
/// our stage.  It does its own event processing.
///
class Widget : public Element
{
	//////////
	/// The wxWindow implementing this widget.
	///
	wxWindow *mWindow;
	
protected:
	//////////
	/// A constructor for Widget subclasses which need to do complicated
	/// widget creation.  Before the subclass exits its constructor, it
	/// must call InitializeWidgetWindow (below).
	///
	/// \param inStage  The stage to which this widget is attached.
	/// \param inName  The name of this widget.
	///
	Widget(Stage *inStage, const wxString &inName,
           Halyard::TCallbackPtr inDispatcher = Halyard::TCallbackPtr());

	//////////
	/// See the constructor without am inWindow argument for details.
	///
	/// \param inWindow  The wxWindow implementing this widget.
	///                 Set the parent of this window to the Stage--
	///                 this will get event-handling and destruction
	///                 hooked up correctly.
	///
	void InitializeWidgetWindow(wxWindow *inWindow);

public:
	//////////
	/// Create a new Widget, and add it to the stage.
	///
	/// \param inStage  The stage to which this widget is attached.
	/// \param inName  The name of this widget.
	/// \param inWindow  The wxWindow implementing this widget.
	///                 Set the parent of this window to the Stage--
	///                 this will get event-handling and destruction
	///                 hooked up correctly.
	///
	Widget(Stage *inStage, const wxString &inName, wxWindow *inWindow);

	//////////
	/// Destroy the widget.
	///
	~Widget();

	//////////
	/// Get the bounding rectangle for the widget.
	///
	virtual wxRect GetRect();

	//////////
	/// Show or hide the widget.
	///
	virtual void Show(bool inShow);

	//////////
	/// Return true if the stage object is shown on the screen.
	///
	virtual bool IsShown();

    //////////
    /// In general, most widgets will want a cursor.
    ///
    virtual bool WantsCursor() const { return true; }

	//////////
	/// Draw an outline around the widget.
	///
	virtual void DrawElementBorder(wxDC &inDC);

	//////////
	/// Pass the keyboard focus to this element.
	///
	virtual void SetFocus();

    //////////
    /// Remove the bounding rectangle of this widget from ioRegion, and
    /// return true to show that we made a change.
    ///
    virtual bool ApplyClippingToStage(wxRegion &ioRegion);

    //////////
    /// Get the accessibility information for this element.
    ///    
    virtual wxAccessible *GetAccessible();
};

#endif // Widget_H
