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

#include "Element.h"
#include "Stage.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  Element Methods
//=========================================================================

Element::Element(Stage *inStage, const wxString &inName,
                 FIVEL_NS TCallbackPtr inDispatcher)
	: mStage(inStage), mName(inName)
{
    ASSERT(mStage);
    ASSERT(mName != "");

    if (inDispatcher) {
        mEventDispatcher = EventDispatcherPtr(new EventDispatcher());
        mEventDispatcher->SetDispatcher(inDispatcher);
    }

	mStage->AddElement(ElementPtr(this));
}

void Element::OperationNotSupported(const char *inOperationName) {
    std::string op(inOperationName);
    std::string name(mName.mb_str());
    THROW("Cannot " + op + " element: " + name);
}

void Element::MoveTo(const wxPoint &inPoint) {
    OperationNotSupported("move");
}

void Element::Show(bool inShow) {
    if (inShow != IsShown()) {
        if (inShow)
            OperationNotSupported("show");
        else
            OperationNotSupported("hide");
    }
}
