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

#include "CommonHeaders.h"
#include "Model.h"
#include "ModelView.h"

using namespace Halyard;
using namespace model;


//=========================================================================
//  View Methods
//=========================================================================

View::View()
    : mObject(NULL), mObjectIsLive(false)
{
}

View::~View() {
    if (mObject)
        mObject->UnregisterView(this);
}

void View::SetObject(Object *inObject) {
    ASSERT(inObject);
    ASSERT(!mObject);
    mObject = inObject;
    mObject->RegisterView(this);
    CallObjectChanged();
}

Object *View::GetObject() {
    ASSERT(mObject && ObjectIsLive());
    return mObject;
}

void View::CallObjectChanged() {
    mObjectIsLive = true;
    ObjectChanged();
}

void View::CallObjectDeleted() {
    mObjectIsLive = false;
    ObjectDeleted();
}

void View::ClearObject() {
    // We are called by the destructor of Object, and we should set any
    // out-of-date pointers to NULL.
    ASSERT(mObject);
    mObject = NULL;
    CallObjectDeleted();
}
