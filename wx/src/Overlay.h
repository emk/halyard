// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Overlay_H
#define Overlay_H

#include "LightweightElement.h"
#include "DrawingArea.h"


//////////
// A overlay is the simplest form of lightweight element.
//
class Overlay : public LightweightElement
{
	DrawingArea mDrawingArea;
	
public:
	Overlay(Stage *inStage, const wxString &inName, const wxRect &inBounds,
			FIVEL_NS TCallback *inDispatch, wxCursor &inCursor,
			bool inHasAlpha);

	virtual bool IsPointInElement(const wxPoint &inPoint);
	virtual void DrawElementBorder(wxDC &inDC);
	virtual DrawingArea *GetDrawingArea() { return &mDrawingArea; }
	virtual void CompositeInto(wxDC &inDC, const wxRect &inClipRect);
	virtual void InvalidateCurrentLocation();
};

#endif // Overlay_H
