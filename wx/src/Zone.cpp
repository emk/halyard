// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "Zone.h"
#include "CommonWxConv.h"
#include "TPoint.h"

USING_NAMESPACE_FIVEL

Zone::Zone(Stage *inStage, const wxString &inName, const TPolygon &inPoly,
		   FIVEL_NS TCallbackPtr inDispatch, wxCursor &inCursor)
    : LightweightElement(inStage, inName, inDispatch, inCursor),
	  mPolygon(inPoly)
{
}

bool Zone::IsPointInElement(const wxPoint &inPoint)
{
    return mPolygon.Contains(WxToTPoint(inPoint));
}

void Zone::DrawElementBorder(wxDC &inDC)
{
	std::vector<TPoint> pts = mPolygon.Vertices();
	std::vector<TPoint>::iterator i;
	TPoint begin, end;

	// TODO - this should be implemented by an iterator over the edges!
	// Implement that iterator!
	for (i = pts.begin(); i != pts.end(); ++i)
	{
		begin = *i;
		end = (i+1) == pts.end() ? *(pts.begin()) : *(i+1);
		
		inDC.DrawLine(begin.X(), begin.Y(), end.X(), end.Y());
	}
}
