// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef LightweightElement_H
#define LightweightElement_H

#include "TInterpreter.h"
#include "Element.h"


//////////
// A lightweight element is basically a "virtual widget" on our stage.  It
// doesn't have an associated wxWindow object; all of its events are passed
// directly to it by the Stage itself.
//
// TODO - Currently, LightweightElements are the only Elements which can
// have event handlers in Scheme.  This is because I haven't figured out
// how to combine the wxWindows event-handling system with the Scheme
// event-handling system we're using.
//
class LightweightElement : public Element {
    EventDispatcher *mDispatcher;
    wxCursor mCursor;
	
public:
	LightweightElement(Stage *inStage, const wxString &inName,
					   FIVEL_NS TCallback *inDispatch, wxCursor &inCursor);
	~LightweightElement();

	virtual bool IsLightWeight() { return true; }

	virtual EventDispatcher *GetEventDispatcher() { return mDispatcher; }

	virtual wxCursor GetCursor() { return mCursor; }
	virtual void SetCursor(wxCursor &inCursor) { mCursor = inCursor; }
};

#endif // LightweightElement_H

