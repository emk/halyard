// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Widget_H
#define Widget_H

#include "Stage.h"
#include "Element.h"

//////////
// A widget represents a full-fledged wxWindow object hanging around on
// our stage.  It does its own event processing.
//
class Widget : public Element
{
	//////////
	// The wxWindow implementing this widget.
	//
	wxWindow *mWindow;
	
public:
	//////////
	// Create a new Widget, and add it to the stage.
	//
	// [in] inStage - The stage to which this widget is attached.
	// [in] inName - The name of this widget.
	// [in] inWindow - The wxWindow implementing this widget.
	//                 Set the parent of this window to the Stage--
	//                 this will get event-handling and destruction
	//                 hooked up correctly.
	//
	Widget(Stage *inStage, const wxString &inName, wxWindow *inWindow);

	//////////
	// Destroy the widget.
	//
	~Widget();

	//////////
	// Get the bounding rectangle for the widget.
	//
	virtual wxRect GetRect() { return mWindow->GetRect(); }

	//////////
	// Return true if the stage object is shown on the screen.
	//
	virtual bool IsShown() { return mWindow->IsShown(); }
};

#endif // Widget_H
