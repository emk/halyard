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

#ifndef EventDispatcher_H
#define EventDispatcher_H

#include "TInterpreter.h"

//////////
/// This class passes events from our GUI to the interpreter.
///
class EventDispatcher : boost::noncopyable, public FIVEL_NS TReloadNotified
{
    FIVEL_NS TCallbackPtr mDispatcher;
	static bool sEnableExpensiveEvents;

	bool EventSetup();
	bool EventCleanup();
    void CheckForVeto(bool &outWasVetoed);

	bool DoSimpleMouseEvent(const char *inType, wxPoint inPosition);

public:
	EventDispatcher();
	~EventDispatcher();

	//////////
	/// Set the event-dispatching callback.
	///
	void SetDispatcher(FIVEL_NS TCallbackPtr inCallback);

	//////////
	/// Notify the EventDispatcher that script is being reloaded.
	///
    void NotifyReloadScriptStarting();

	//////////
	/// Turn "expensive" events--idle events, mouse moved events--on or
	/// off.  These tend to generate a lot of garbage for the Scheme
	/// GC to clean up.
	///
	static void EnableExpensiveEvents(bool inEnable);

    //////////
    /// Dispatch an event asking for menus and buttons related to
    /// inCommandName to be updated.  Similar to wxWindows EVT_UPDATE_UI.
    ///
    bool DoEventUpdateUI(const wxString &inCommandName);

	//////////
	/// Dispatch a mouse-down event.
	///
	bool DoEventLeftDown(wxMouseEvent &inEvent, bool inIsDoubleClick);

	///////////
	/// Dispatch a mouse-up event.
	///
	bool DoEventLeftUp(wxMouseEvent &inEvent);

	//////////
	/// Dispatch a mouse-enter event.
	///
	bool DoEventMouseEnter(wxPoint inPosition);

	//////////
	/// Dispatch a mouse-leave event.
	///
	bool DoEventMouseLeave(wxPoint inPosition);

	//////////
	/// Dispatch a character event.  Return true if the event was handled.
	///
	bool DoEventChar(wxKeyEvent &inEvent);

	//////////
	/// Dispatch an idle event.
	///
	bool DoEventIdle(wxIdleEvent &inEvent);
	
	//////////
	/// Dispatch a mouse move event.
	///
	bool DoEventMouseMoved(wxMouseEvent &inEvent);

    //////////
    /// Dispatch a BrowserNavigate event.  This occurs before loading a new
    /// page, and it may be vetoed.
    ///
    bool DoEventBrowserNavigate(const wxString &inUrl, bool &outWasVetoed);

    //////////
    /// Dispatch a PageChanged event.  This occurs whenever the browser
    /// has loaded a new page, or accomplished some similar task.
    ///
    bool DoEventBrowserPageChanged(const wxString &inUrl);

    //////////
    /// Dispatch a TitleChanged event.  This occurs whenever the browser
    /// window title changes.
    ///
    bool DoEventBrowserTitleChanged(const wxString &inTitle);

    //////////
    /// Dispatch a SetStatusText event.
    ///
    bool DoEventStatusTextChanged(const wxString &inText);
    
    //////////
    /// Dispatch a ProgressChanged event.
    ///
    bool DoEventProgressChanged(bool inIsActive, double inPortionCompleted);

    //////////
    /// Dispatch a MediaFinished event.
    ///
    bool DoEventMediaFinished();
};

typedef shared_ptr<EventDispatcher> EventDispatcherPtr;

#endif // EventDispatcher_H
