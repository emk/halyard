// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TRect.h"
#include "TPoint.h"
#include "GraphicsTools.h"

wxRect TToWxRect(const TRect &inRect);
wxPoint TToWxPoint(const TPoint &inPoint);
TPoint WxToTPoint(const wxPoint &inPoint);
wxPoint GetPos(const TRect &inRect);
wxSize GetSize(const TRect &inRect);
wxColour GraphicsToolsToWxColor(GraphicsTools::Color inColor);

