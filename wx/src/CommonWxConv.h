// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TRect.h"
#include "TPoint.h"

wxRect TToWxRect(const TRect &inRect);
TRect WxToTRect(const wxRect &inRect);
wxPoint TToWxPoint(const TPoint &inPoint);
TPoint WxToTPoint(const wxPoint &inPoint);
wxPoint GetPos(const TRect &inRect);
wxSize GetSize(const TRect &inRect);
wxColour GraphicsToolsToWxColor(const GraphicsTools::Color &inColor);

