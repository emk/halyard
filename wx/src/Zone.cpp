// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include <vector>

#include "TCommon.h"
#include "Zone.h"
#include "EventDispatcher.h"
#include "CommonWxConv.h"
#include "TLogger.h"
#include "TPoint.h"

USING_NAMESPACE_FIVEL

Zone::Zone(Stage *inStage, const wxString &inName, const TPolygon &inPoly,
		   FIVEL_NS TCallback *inDispatch, wxCursor &inCursor)
    : Element(inStage, inName), mPolygon(inPoly), mCursor(inCursor)
{
    ASSERT(inDispatch);
	mDispatcher = new EventDispatcher();
	mDispatcher->SetDispatcher(inDispatch);
}

Zone::~Zone()
{
	// XXX - The callback in this dispatcher may still be active!
    delete mDispatcher;
}

wxRect Zone::GetRect()
{ 
	return TToWxRect(mPolygon.Bounds()); 
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

	// XXX - this should be implemented by an iterator over the edges!
	// Implement that iterator!
	for (i = pts.begin(); i != pts.end(); ++i)
	{
		begin = *i;
		end = (i+1) == pts.end() ? *(pts.begin()) : *(i+1);
		
		inDC.DrawLine(begin.X(), begin.Y(), end.X(), end.Y());
	}
}
