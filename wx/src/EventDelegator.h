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

#ifndef EventDelegator_H
#define EventDelegator_H

/// Delegates menu-selection events from one wxEvtHandler object to another.
/// This allows wxFrame objects to forward events to various child wxWindow
/// objects that actually know how to process them, similar to the old
/// "bureaucrat" system in Metrowerks PowerPlant.
class EventDelegator {
    wxEvent *mCurrentEvent;
    wxEvtHandler *mDelegate;

public:
    EventDelegator();
    void SetDelegate(wxEvtHandler *delegate);
    bool DelegateEvent(wxEvent& event, bool *outResult);
};

#endif // EventDelegator_H
