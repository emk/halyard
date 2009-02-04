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

// Various things which need to happen before we include wxWidgets.
#include "TWarnings.h"

// Commonly-used wxWindows headers.
#include <wx/wx.h>
#include <wx/xrc/xmlres.h>

// This needs to appear at least once in our project, in either a header or
// a cpp file.  See:
//
//   http://www.wxwidgets.org/wiki/index.php/MSVC_.NET_Setup_Guide
//
// ...for the details.  Note that there's a corresponding "#define
// wxUSE_NO_MANIFEST 1" in application.rc.
#if defined(__WXMSW__) && !defined(__WXWINCE__)
#pragma comment(linker, "\"/manifestdependency:type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='X86' publicKeyToken='6595b64144ccf1df'\"")
#endif

// Commonly-used headers from our portable runtime library.
#include "CommonHeaders.h"
