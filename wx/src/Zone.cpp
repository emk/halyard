// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "Zone.h"
#include "EventDispatcher.h"

USING_NAMESPACE_FIVEL

Zone::Zone(Stage *inStage, const wxString &inName, const wxRect &inBounds,
		   FIVEL_NS TCallback *inDispatch, wxCursor &inCursor)
    : Element(inStage, inName), mBounds(inBounds), mCursor(inCursor)
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

bool Zone::IsPointInElement(const wxPoint &inPoint)
{
    return mBounds.Inside(inPoint);
}

