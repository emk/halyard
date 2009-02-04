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

#include "AppHeaders.h"
#include "DrawingContextStack.h"
#include "Stage.h"
#include "Element.h"
#include "DrawingArea.h"

using namespace Halyard;

bool DrawingContextStack::ContainsElement(ElementPtr inElement) {
    ElementStack::iterator i = mDrawingContextStack.begin();
    for (; i != mDrawingContextStack.end(); ++i)
        if (*i == inElement)
            return true;
    return false;
}

DrawingArea *DrawingContextStack::GetCurrentDrawingArea() {
    if (IsEmpty()) {
        return mStage->GetBackgroundDrawingArea();
    } else {
        DrawingArea *area = mDrawingContextStack.back()->GetDrawingArea();
        ASSERT(area != NULL);
        return area;
    }
}

void DrawingContextStack::PushDrawingContext(ElementPtr inElement) {
    DrawingArea *area = inElement->GetDrawingArea();
    if (!area)
        THROW("Element has no associated drawing context");
    mDrawingContextStack.push_back(inElement);
}

void DrawingContextStack::PopDrawingContext(ElementPtr inElement) {
    ASSERT(!IsEmpty());
    if (inElement != mDrawingContextStack.back())
        gLog.FatalError("Mismatched drawing context push/pop");
    mDrawingContextStack.pop_back();
}
