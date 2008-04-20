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

#ifndef LightweightAccessible_H
#define LightweightAccessible_H

#include <wx/access.h>

#if wxUSE_ACCESSIBILITY

/// Custom accessibility interface for LightweightElement.  Used by
/// screen-readers and other accessibility tools to learn about a specific
/// LightweightElement on the Stage.
class LightweightAccessible : public wxAccessible {
    LightweightElement *mElement;

public:
    LightweightAccessible(LightweightElement *element);

    wxAccStatus GetParent(wxAccessible** parent);
    wxAccStatus GetChildCount(int* childCount);
    wxAccStatus GetName(int childId, wxString* name);
    wxAccStatus GetRole(int childId, wxAccRole* role);
    wxAccStatus GetState(int childId, long* state);
    wxAccStatus GetFocus(int* childId, wxAccessible** child);
    wxAccStatus GetLocation(wxRect& rect, int elementId);
};

#endif // wxUSE_ACCESSIBILITY
#endif // LightweightAccessible_H
