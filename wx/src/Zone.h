// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Zone_H
#define Zone_H

#include "TInterpreter.h"
#include "Element.h"
#include "TPolygon.h"

class Stage;

//////////
// A zone is basically a "virtual widget" on our stage.  It doesn't
// have an associated wxWindow object; all of its events are passed directly
// to it by the Stage itself.
//
// TODO - Currently, Zones are the only Elements which can have event
// handlers in Scheme.  This is because I haven't figured out how to
// combine the wxWindows event-handling system with the Scheme
// event-handling system we're using.
//
class Zone : public Element
{
	TPolygon mPolygon;
	EventDispatcher *mDispatcher;
	wxCursor mCursor;
	
public:
	Zone(Stage *inStage, const wxString &inName, const TPolygon &inPoly,
		 FIVEL_NS TCallback *inDispatch, wxCursor &inCursor);
	~Zone();

	virtual bool IsLightWeight() { return true; }

	virtual wxRect GetRect();
	virtual bool IsPointInElement(const wxPoint &inPoint);

	virtual EventDispatcher *GetEventDispatcher() { return mDispatcher; }

	virtual wxCursor GetCursor() { return mCursor; }
	virtual void SetCursor(wxCursor &inCursor) { mCursor = inCursor; }

	virtual void DrawElementBorder(wxDC &inDC);
};

#endif // Zone_H
