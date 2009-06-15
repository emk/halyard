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
#include "Node.h"

using namespace Halyard;


//=========================================================================
//  Node Methods
//=========================================================================

Node::Node(const wxString &inName, Halyard::TCallbackPtr inDispatcher)
    : mName(inName), mLogName(inName.mb_str())
{
    ASSERT(mName != wxT(""));

    if (inDispatcher) {
        mEventDispatcher = EventDispatcherPtr(new EventDispatcher());
        mEventDispatcher->SetDispatcher(inDispatcher);
    }
}

void Node::OperationNotSupported(const char *inOperationName) {
    std::string op(inOperationName);
    std::string name(mName.mb_str());
    THROW("Cannot " + op + " node: " + name);
}

void Node::Show(bool inShow) {
    if (inShow != IsShown()) {
        if (inShow)
            OperationNotSupported("show");
        else
            OperationNotSupported("hide");
    }
}
