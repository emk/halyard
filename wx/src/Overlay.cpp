// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "Overlay.h"
#include "CommonWxConv.h"
#include "TPoint.h"
#include "Stage.h"

USING_NAMESPACE_FIVEL

Overlay::Overlay(Stage *inStage, const wxString &inName,
		 const wxRect &inBounds, FIVEL_NS TCallback *inDispatch,
		 wxCursor &inCursor, bool inHasAlpha)
    : LightweightElement(inStage, inName, inDispatch, inCursor),
      mDrawingArea(inStage, inBounds, inHasAlpha)
{
}

bool Overlay::IsPointInElement(const wxPoint &inPoint) {
	wxRect bounds = mDrawingArea.GetBounds();
    if (!bounds.Inside(inPoint)) {
		// Outside our bounding box.
		return false; 
	} else if (!mDrawingArea.HasAlpha()) {
		// We're opaque, so we only need to check the bounding box.
		return true;
	} else {
		// We have an alpha channel.  We only consider points to be inside
		// the overlay if they contain graphical data.
		GraphicsTools::Color c =
			mDrawingArea.GetPixel(inPoint.x - bounds.x, inPoint.y - bounds.y);
		return !c.IsCompletelyTransparent();
	}
}

void Overlay::DrawElementBorder(wxDC &inDC) {
    inDC.DrawRectangle(mDrawingArea.GetBounds());
}

void Overlay::CompositeInto(wxDC &inDC, const wxRect &inClipRect) {
    mDrawingArea.CompositeInto(inDC, inClipRect);
}

void Overlay::InvalidateCurrentLocation() {
	GetStage()->InvalidateRect(mDrawingArea.GetBounds());
}
