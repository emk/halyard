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

#include "AppHeaders.h"
#include "EventDelegator.h"

using namespace Halyard;

/// Create a new EventDelegator.
EventDelegator::EventDelegator()
    : mCurrentEvent(NULL), mDelegate(NULL)
{
}

/// Set the wxEvtHandler which should process our events.  This may be
/// a child object; we prevent loops.  To disable delegation, pass NULL.
void EventDelegator::SetDelegate(wxEvtHandler *delegate) {
    mDelegate = delegate;
}

/// Delegate some events to another object, if we have one.
/// 
/// \param event The event to delegate.
/// \param outResult [out] The return value for ProcessEvent if we delegated.
/// \return True if the event was delegated, false if the caller should
///         handle it normally.
bool EventDelegator::DelegateEvent(wxEvent& event, bool *outResult) {
    // We can't delegate if nobody's listening.
    if (mDelegate == NULL)
        return false;

    // We can't delegate if we've already delegated this event once (our child
    // has apparently delegated it back to us).
    if (&event == mCurrentEvent)
        return false;

    // We shouldn't delegate if this isn't a command-related event.
    if (event.GetEventType() != wxEVT_UPDATE_UI &&
        event.GetEventType() != wxEVT_COMMAND_MENU_SELECTED)
        return false;

    // We can actually delegate this event!  Save our new value of
    // mCurrentEvent.
	wxEvent *prev_event = mCurrentEvent;
	try {
        mCurrentEvent = &event;
        *outResult = mDelegate->ProcessEvent(event);
    } catch (...) {
        mCurrentEvent = prev_event;
        throw;
    }
    mCurrentEvent = prev_event;
    return true;
}
