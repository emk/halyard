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


