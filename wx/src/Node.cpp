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
#include "HalyardApp.h"
#include "Stage.h"
#include "StageFrame.h"
#include "ElementsPane.h"

using namespace Halyard;


//=========================================================================
//  Node Methods
//=========================================================================

Node::Node(const wxString &inName, Halyard::TCallbackPtr inDispatcher)
    : mName(inName), mLogName(inName.mb_str())
{
    ASSERT(mName != wxT(""));

    // If we're not the root node, look up our parent node.
    if (mName != wxT("/")) {
        size_t last_slash(mName.rfind(wxT("/")));
        wxString parent_name;
        if (last_slash == 0)
            parent_name = wxT("/");
        else
            parent_name = mName.substr(0, last_slash);
        mParent = wxGetApp().GetStage()->FindNode(parent_name);
        ASSERT(mParent);
    }

    if (inDispatcher) {
        // Initialize a named pointer on a separate line to prevent leaks.
        // We should do this pretty much everywhere we use shared_ptr.
        EventDispatcherPtr ptr(new EventDispatcher());
        mEventDispatcher = ptr;
        mEventDispatcher->SetDispatcher(inDispatcher);
    }
}

void Node::OperationNotSupported(const char *inOperationName) {
    std::string op(inOperationName);
    std::string name(mName.mb_str());
    THROW("Cannot " + op + " node: " + name);
}

wxString Node::GetDisplayName() {
    if (IsRootNode()) {
        return mName;
    } else {
        size_t last_slash(mName.rfind(wxT("/")));
        return mName.substr(last_slash + 1);
    }
}

void Node::Show(bool inShow) {
    if (inShow != IsShown()) {
        if (inShow)
            OperationNotSupported("show");
        else
            OperationNotSupported("hide");
    }
}

void Node::Register() {
    NodePtr as_shared(shared_from_this());
    wxGetApp().GetStageFrame()->GetElementsPane()->RegisterNode(as_shared);
}

void Node::Unregister() {
    NodePtr as_shared(shared_from_this());
    wxGetApp().GetStageFrame()->GetElementsPane()->UnregisterNode(as_shared);
}

void Node::RegisterChildElement(ElementPtr inElem) {
    ASSERT(std::find(mElements.begin(), mElements.end(), inElem) ==
           mElements.end());
    mElements.push_back(inElem);
}

void Node::UnregisterChildElement(ElementPtr inElem) {
    ElementList::iterator found =
        std::find(mElements.begin(), mElements.end(), inElem);
    ASSERT(found != mElements.end());
    mElements.erase(found);
}
