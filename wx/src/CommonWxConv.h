// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#ifndef CommonWxConv_H
#define CommonWxConv_H

#include <wx/variant.h>
#include "TRect.h"
#include "TPoint.h"

wxRect TToWxRect(const Halyard::TRect &inRect);
Halyard::TRect WxToTRect(const wxRect &inRect);
wxPoint TToWxPoint(const Halyard::TPoint &inPoint);
Halyard::TPoint WxToTPoint(const wxPoint &inPoint);
wxPoint GetPos(const Halyard::TRect &inRect);
wxSize GetSize(const Halyard::TRect &inRect);
wxColour GraphicsToolsToWxColor(const GraphicsTools::Color &inColor);
wxVariant TToWxValue(const Halyard::TValue &value);
Halyard::TValue WxToTValue(const wxVariant &value);
wxString ToWxString(const std::string &inString);
std::string ToStdString(const wxString &inString);

#endif // CommonWxConv_H
