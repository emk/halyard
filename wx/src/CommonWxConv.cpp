// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include "CommonWxConv.h"
#include "FiveLApp.h"
#include "Stage.h"

wxRect TToWxRect(const TRect &inRect) {
	return wxRect(GetPos(inRect), GetSize(inRect));
}

TRect WxToTRect(const wxRect &inRect) {
	// Incredibly rude argument order!
	return TRect(inRect.y, inRect.x, 
				 inRect.y + inRect.height, inRect.x + inRect.width);
}

wxPoint TToWxPoint(const TPoint &inPoint)
{
	return wxPoint(inPoint.X(), inPoint.Y());
}

TPoint WxToTPoint(const wxPoint &inPoint)
{
	return TPoint(inPoint.x, inPoint.y);
}

wxPoint GetPos(const TRect &inRect)
{
	return wxPoint(inRect.Left(), inRect.Top());
}

wxSize GetSize(const TRect &inRect)
{
	// Our rectangles *exclude* their right and bottom edges, like C++
	// iterators.  This is the sanest way to do things, and it works well
	// with wxWindows.
	return wxSize(inRect.Right() - inRect.Left(),
				  inRect.Bottom() - inRect.Top());
}

wxColour GraphicsToolsToWxColor(const GraphicsTools::Color &inColor)
{
    if (!inColor.IsCompletelyOpaque())
        gDebugLog.Caution("Removing alpha channel from color");
    return wxColour(inColor.red, inColor.green, inColor.blue);
}


