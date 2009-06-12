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

#include "AppHeaders.h"
#include "Stage.h"
#include "StageAccessible.h"
#include "Element.h"

using namespace Halyard;


//=========================================================================
//  Stage Accessibility Support
//=========================================================================
//  See LightweightAccessible.cpp for notes and documentation on
//  accessibility.  There's a lot of background reading you'll need to do
//  before any of this really makes sense.

#if wxUSE_ACCESSIBILITY

/// Create a new StageAccessible object.
StageAccessible::StageAccessible(Stage *stage)
    : mStage(stage)
{
}

/// Get the number of children of the stage.
wxAccStatus StageAccessible::GetChildCount(int* childCount) {
    *childCount = mStage->GetAccessibleChildCount();
    return wxACC_OK;
}

/// Get the specified child of the stage.  (The index is 1-based.)  May
/// return NULL if an element should be ignored by the accessibility
/// framework.
wxAccStatus StageAccessible::GetChild(int childId, wxAccessible** child) {
    ElementPtr childPtr = mStage->GetAccessibleChild(childId-1);
    *child = childPtr->GetAccessible(); // May be NULL, but that's OK.
    return wxACC_OK;
}

#endif // wxUSE_ACCESSIBILITY
