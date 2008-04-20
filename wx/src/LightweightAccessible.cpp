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

#include "AppHeaders.h"
#include "LightweightElement.h"
#include "LightweightAccessible.h"
#include "Stage.h"

using namespace Halyard;


//=========================================================================
//  LightweightElement Accessibility Support
//=========================================================================
//  We want to support screen-readers and other devices for blind and
//  vision-impaired users.  We do this using wxWidgets' wxAccessible class,
//  which is in turn based on Microsoft Active Accessibility (MSAA) version
//  1.3.  Unfortunately, MSAA provides a fragile and poorly-documented way
//  of talking to screen readers.  Even more unfortunately, there's nothing
//  else that's any better, short of directly working with the screen
//  reader companies and building a custom interface.
//
//  MSAA is a little bit like QuickTime, in that both are black arts.  In
//  neither case can we really rely on the official documentation, and the
//  ultimate test of a given approach is whether it works (and continues to
//  work) in practice.
//
//  Some good news: We're not the first people to go down this path.  In
//  particular, the Mozilla team has been here before, and they took very
//  good notes on what actually works.  Read this before continuing:
//
//    http://www.mozilla.org/access/windows/msaa-server
//
//  Among other things, this document explains which portions of the MSAA
//  API to implement, which portions MUST be ignored to prevent crashes,
//  and how everything actually fits together.  Since Mozilla is one of the
//  largest and best-documented users of MSAA, we'll have the best luck if
//  we follow its conventions (and those of IE, on which the Mozilla MSAA
//  support is apparently based).
//
//  Next, read these:
//
//    http://www.wxwidgets.org/manuals/2.6.3/wx_wxaccessible.html
//    http://www.wxwidgets.org/docs/access.htm (may be slightly out of date)
//
//  For comparison purposes, here's how Adobe has implemented accessibility:
//
//    http://www.adobe.com/accessibility/
//
//  In general, supporting MSAA isn't enough to make screen readers work.
//  You must also instruct the screen reader to use MSAA support for certain
//  windows on the screen.
//
//  For quick & dirty debugging, I'm especially fond of Microsoft's
//  AccExplorer and related tools.  For serious work, you'll need a screen
//  reader license.

#if wxUSE_ACCESSIBILITY

/// Create a new LightweightAccessible object.
LightweightAccessible::LightweightAccessible(LightweightElement *element)
    : mElement(element)
{
}

/// Get the wxAccessible object for this Element's parent.
wxAccStatus LightweightAccessible::GetParent(wxAccessible** parent) {
    *parent = mElement->GetStage()->GetAccessible();
    return wxACC_OK;
}

/// Get the number of children of this object.
wxAccStatus LightweightAccessible::GetChildCount(int* childCount) {
    *childCount = 0;
    return wxACC_OK;
}

/// Get the name of this object.  This is the string that will normally
/// be read by the 
wxAccStatus LightweightAccessible::GetName(int childId, wxString* name) {
    ASSERT(childId == 0);
    *name = mElement->GetName();
    return wxACC_OK;
}

/// Get the "role" of this object: Is it a button, static text, or
/// something else?
wxAccStatus LightweightAccessible::GetRole(int childId, wxAccRole* role) {
    ASSERT(childId == 0);
    *role = wxROLE_SYSTEM_PANE;
    return wxACC_OK;
}

/// Get the current state of this object.  This is a large bitfield
/// containing many flags.
wxAccStatus LightweightAccessible::GetState(int childId, long* state) {
    ASSERT(childId == 0);
    *state = 0;
    return wxACC_OK;
}

/// Return the currently-focused child of this object.
wxAccStatus LightweightAccessible::GetFocus(int* childId, wxAccessible** child)
{
    ASSERT(*childId == 0);
    *child = NULL;
    return wxACC_OK;
}

/// Get the bounding rectangle of this object on the screen.
wxAccStatus LightweightAccessible::GetLocation(wxRect& rect, int elementId) {
    ASSERT(elementId == 0);
    return wxACC_NOT_IMPLEMENTED;
}

#endif // wxUSE_ACCESSIBILITY
