// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Zone_H
#define Zone_H

#include "LightweightElement.h"
#include "TPolygon.h"


//////////
// A zone is the simplest form of lightweight element.
//
class Zone : public LightweightElement
{
	TPolygon mPolygon;
	
public:
	Zone(Stage *inStage, const wxString &inName, const TPolygon &inPoly,
		 FIVEL_NS TCallbackPtr inDispatch, wxCursor &inCursor);

	virtual bool IsPointInElement(const wxPoint &inPoint);

	virtual void DrawElementBorder(wxDC &inDC);
};

#endif // Zone_H
