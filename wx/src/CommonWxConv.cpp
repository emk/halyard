// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#include "TamaleHeaders.h"

#include "CommonWxConv.h"
#include "FiveLApp.h"
#include "Stage.h"

USING_NAMESPACE_FIVEL

wxRect TToWxRect(const TRect &inRect) {
	return wxRect(GetPos(inRect), GetSize(inRect));
}

TRect WxToTRect(const wxRect &inRect) {
	// Incredibly rude argument order!
	return TRect(inRect.x, inRect.y, 
				 inRect.x + inRect.width, inRect.y + inRect.height); 
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

/// Convert a TValue into a wxWidgets wxVariant.
wxVariant TToWxValue(const TValue &value) {
    switch (value.GetType()) {
        case TValue::TYPE_STRING:
            return wxVariant(std::string(value).c_str());
            
        case TValue::TYPE_BOOLEAN:
            return wxVariant(bool(value));
            
        case TValue::TYPE_LONG:
            return wxVariant(long(value));

        case TValue::TYPE_NULL:
        case TValue::TYPE_SYMBOL:
        case TValue::TYPE_ULONG:
        case TValue::TYPE_DOUBLE:
        case TValue::TYPE_POINT:
        case TValue::TYPE_RECT:
        case TValue::TYPE_COLOR:
        case TValue::TYPE_LIST:
        case TValue::TYPE_POLYGON:
        case TValue::TYPE_CALLBACK:
        case TValue::TYPE_PERCENT:
        default:
            THROW("Can't convert TValue to wxVariant");
    }
}

/// Convert a wxWidgets wxVariant into a TValue.
TValue WxToTValue(const wxVariant &value) {
    wxString type(value.GetType());
    if (type == "string") {
        return TValue(value.GetString());
    } else if (type == "bool") {
        return TValue(value.GetBool());        
    } else if (type == "long") {
        return TValue(value.GetLong());
    } else {
        THROW("Can't convert wxVariant to TValue");
    }
}
