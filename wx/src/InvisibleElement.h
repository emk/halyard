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

#ifndef InvisibleElement_H
#define InvisibleElement_H

#include "Element.h"

//////////
/// An invisible element never actually gets displayed.  Most of these are
/// currently associated with audio, but there may be more types in the
/// future.
///
class InvisibleElement : public Element {
public:
	InvisibleElement(Stage *inStage, const wxString &inName,
                     Halyard::TCallbackPtr inDispatcher =
                         Halyard::TCallbackPtr())
        : Element(inStage, inName, inDispatcher) {}

    virtual wxRect GetRect() { return wxRect(0, 0, 0, 0); }
    virtual bool HasVisibleRepresentation() { return false; }
    virtual bool IsShown() { return false; }
    virtual bool IsLightWeight() { return true; }
    virtual bool IsPointInElement(const wxPoint &inPoint) { return false; }
};

#endif // InvisibleElement_H
