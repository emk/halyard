// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Zone_H
#define Zone_H

#include "TInterpreter.h"

#include "Stage.h"
#include "Element.h"

//////////
// A zone is basically a "virtual widget" on our stage.  It doesn't
// have an associated wxWindow object; all of its events are passed directly
// to it by the Stage itself.
//
class Zone : public Element
{
	wxRect mBounds;
	FIVEL_NS TCallback *mAction;
	
public:
	Zone(Stage *inStage, const wxString &inName, const wxRect &inBounds,
		 FIVEL_NS TCallback *inAction);
	~Zone();

	virtual bool IsLightWeight() { return true; }

	virtual wxRect GetRect() { return mBounds; }
	virtual bool IsPointInElement(const wxPoint &inPoint);
	virtual void Click();
};

#endif // Zone_H
