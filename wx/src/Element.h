// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Element_H
#define Element_H

#include <map>
#include <string>
#include <wx/wx.h>

#include "TInterpreter.h"

class Stage;
class EventDispatcher;


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
	// A list of event handlers supported by this element.
	//
	typedef std::map<std::string,FIVEL_NS TCallback*> EventHandlerMap;

	//////////
	// The stage on which this element appears.
	//
	Stage *mStage;

	//////////
	// The name of this element.  Must be unique on any given card. 
	//
	wxString mName;

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
	// Return the bounding box of the element.  Used for drawing
	// borders around the element.
	//
	virtual wxRect GetRect() = 0;

	//////////
	// Return true if the element can be shown.
	//
	virtual bool HasVisibleRepresentation() { return true; }

	//////////
	// Return true if the element is shown on the screen.
	//
	virtual bool IsShown() { return true; }

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
	virtual void DrawElementBorder(wxDC &inDC);
};

#endif // Element_H
