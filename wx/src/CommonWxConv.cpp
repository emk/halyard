// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonWxConv.h"
#include "FiveLApp.h"
#include "Stage.h"

wxRect TToWxRect(const TRect &inRect)
{
	return wxRect(wxPoint(inRect.Left(), inRect.Top()),
				  wxPoint(inRect.Right() - 1, inRect.Bottom() - 1));
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
	return wxSize(1 + inRect.Right() - inRect.Left(),
				  1 + inRect.Bottom() - inRect.Top());
}

wxColour GraphicsToolsToWxColor(GraphicsTools::Color inColor)
{
	// Translate a color using the official translator function.
	return wxGetApp().GetStage()->GetColor(inColor);
}


