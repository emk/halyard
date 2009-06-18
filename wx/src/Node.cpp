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
#include <boost/foreach.hpp>
#include "Node.h"
#include "Element.h"
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

bool Node::IsRealChild(ElementPtr inElem) {
    return (inElem->GetParent().get() == this);
}

bool Node::IsChildForPurposeOfZOrderAndVisibility(ElementPtr inElem) {
    return IsRealChild(inElem) && !inElem->HasLegacyZOrderAndVisibility();
}

wxString Node::GetDisplayName(bool *outIsAnonymous) {
    wxString result;
    bool is_anonymous = false;
    if (IsRootNode()) {
        result = mName;
    } else {
        size_t last_slash(mName.rfind(wxT("/")));
        result = mName.substr(last_slash + 1);

        // Names of the form "%%anon-(.*)-\d+" are treated as anonymous
        // nodes from the user's perspective.  We only display the (.*)
        // portion.
        wxString anon_prefix(wxT("%%anon-"));
        size_t anon_prefix_size(anon_prefix.size());
        size_t anon_suffix_start(result.rfind(wxT("-")));
        if (result.substr(0, anon_prefix_size) == anon_prefix &&
            anon_suffix_start != wxString::npos &&
            anon_prefix_size < anon_suffix_start)
        {
            wxString tmp = result.substr(anon_prefix_size,
                                         anon_suffix_start - anon_prefix_size);
            result = tmp;
            is_anonymous = true;
        }
    }

    // Return our results.
    if (outIsAnonymous)
        *outIsAnonymous = is_anonymous;
    return result;
}

void Node::Show(bool inShow) {
    if (inShow != IsShown()) {
        // Let our subclasses do the actual showing and hiding.
        DoShow(inShow);

        // Notify the Stage and the ElementsPane.
        wxGetApp().GetStage()->NotifyNodesChanged();
        wxGetApp().GetStageFrame()->GetElementsPane()->
            NotifyNodeStateChanged(shared_from_this());
    }
}

void Node::DoShow(bool inShow) {
    if (inShow)
        OperationNotSupported("show");
    else
        OperationNotSupported("hide");
}

void Node::Register() {
    NodePtr as_shared(shared_from_this());
    wxGetApp().GetStageFrame()->GetElementsPane()->RegisterNode(as_shared);
}

void Node::Unregister() {
    NodePtr as_shared(shared_from_this());
    wxGetApp().GetStageFrame()->GetElementsPane()->UnregisterNode(as_shared);
    
}

void Node::RecursivelyReregisterWithElementsPane(ElementsPane *inPane) {
    NodePtr as_shared(shared_from_this());
    inPane->RegisterNode(as_shared);
    BOOST_FOREACH(ElementPtr elem, mElements)
        if (IsRealChild(elem))
            elem->RecursivelyReregisterWithElementsPane(inPane);
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
