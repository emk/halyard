// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Element_H
#define Element_H

class Stage;
class EventDispatcher;
class DrawingArea;


//////////
// A Element is something which appears on a stage--typically either
// a Zone or an full-fledged widget.  Elements are either "lightweight",
// meaning they need help from the stage to process events, or they are
// heavyweight and use wxWindows to process their events.  Zones are
// lightweight; widgets are heavyweight.
//
// This class has a "fat" interface--a number of methods are only useful
// if IsLightWeight returns true.  This allows us to avoid using RTTI,
// but is otherwise a slightly odd design.
//
class Element
{
	//////////
	// The stage on which this element appears.
	//
	Stage *mStage;

	//////////
	// The name of this element.  Must be unique on any given card. 
	//
	wxString mName;

    //////////
    // Throw an error saying inOperationName is not allowed.
    //
    void OperationNotSupported(const char *inOperationName);

public:
	//////////
	// Create a new Element and attach it to the specified stage.
	// The stage is responsible for deleting the element.
	//
	Element(Stage *inStage, const wxString &inName);

	virtual ~Element() {}
	
	//////////
	// Return the stage on which the element appears.
	//
	Stage *GetStage() { return mStage; }

	//////////
	// Return the name of the element.  Should be unique on any
	// given card.
	//
	wxString GetName() { return mName; }

	//////////
	// Return true if the element can be shown.
	//
	virtual bool HasVisibleRepresentation() { return true; }

	//////////
	// Return true if the element is shown on the screen.
	//
	virtual bool IsShown() { return true; }

	//////////
	// Show or hide the widget.
	//
	virtual void Show(bool inShow);

	//////////
	// Does this element need to receive events from the Stage?
	//
	virtual bool IsLightWeight() { return false; }

	//////////
	// Is the specified point in the element?  If this function ever returns
	// true, then GetEventDispatcher must not return NULL.
	// NOT USEFUL UNLESS IsLightWeight RETURNS TRUE.
	//
	virtual bool IsPointInElement(const wxPoint &inPoint) { return false; }

    //////////
    // Move the element to the specified location.
    //
    virtual void MoveTo(const wxPoint &inPoint);

	//////////
	// Get the event dispatcher for the current element.
	// NOT USEFUL UNLESS IsLightWeight RETURNS TRUE.
	//
	virtual EventDispatcher *GetEventDispatcher()
		{ return NULL; }
	
	//////////
	// Get an appropriate cursor for this object.
	//
	virtual wxCursor GetCursor() { return wxCursor(wxCURSOR_HAND); }

	/////////
	// Draw the element to the specified DC
	//
	virtual void DrawElementBorder(wxDC &inDC) {}

	//////////
	// Return the DrawingArea associated with this element, if any.
	//
	virtual DrawingArea *GetDrawingArea() { return NULL; }

	//////////
	// Composite our data into the specified DC.
	//
	// [in] inDC - The compositing DC.
	// [in] inClipRect - The rectangle (in terms of inDC co-ordinates)
	//                   which we're updating.
	//
	virtual void CompositeInto(wxDC &inDC, const wxRect &inClipRect) {}
};

#endif // Element_H
