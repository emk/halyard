// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
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

#ifndef PopUpMenu_H
#define PopUpMenu_H

#include "Widget.h"

class wxChoice;

BEGIN_NAMESPACE_HALYARD

/// A list box widget with multiple items.
class PopUpMenu : public Widget {
    wxChoice *mChoice;

public:
    PopUpMenu(const wxString &inName, 
              Halyard::TCallbackPtr inDispatch,
              const wxRect &inBounds);

    /// Add an item to this pop up menu.
    void AddItem(const wxString &inValue);

    /// Get the index of the currently-selected item.
    int GetSelection() const;
};

END_NAMESPACE_HALYARD

#endif // PopUpMenu_H
