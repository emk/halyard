// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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
#include "Zone.h"
#include "CommonWxConv.h"
#include "TPoint.h"

using namespace Halyard;

Zone::Zone(Stage *inStage, const wxString &inName, const TPolygon &inPoly,
		   Halyard::TCallbackPtr inDispatch, const std::string &inCursorName)
    : LightweightElement(inStage, inName, inDispatch, inCursorName),
	  mPolygon(inPoly)
{
}

bool Zone::IsPointInElement(const wxPoint &inPoint) {
    return mPolygon.Contains(WxToTPoint(inPoint));
}

void Zone::MoveTo(const wxPoint &inPoint) {
    mPolygon.MoveTo(WxToTPoint(inPoint));
}

void Zone::DrawElementBorder(wxDC &inDC) {
	std::vector<TPoint> pts = mPolygon.Vertices();
	std::vector<TPoint>::iterator i;
	TPoint begin, end;

	// TODO - this should be implemented by an iterator over the edges!
	// Implement that iterator!
	for (i = pts.begin(); i != pts.end(); ++i) {
		begin = *i;
		end = (i+1) == pts.end() ? *(pts.begin()) : *(i+1);
		
		inDC.DrawLine(begin.X(), begin.Y(), end.X(), end.Y());
	}
}

void Zone::SetShape(const Halyard::TPolygon &inPoly) {
    mPolygon = inPoly;
}
