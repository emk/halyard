// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef EventDispatcher_H
#define EventDispatcher_H

//////////
// This class passes events from our GUI to the interpreter.
//
class EventDispatcher : boost::noncopyable
{
    TCallbackPtr mDispatcher;
	static bool sEnableExpensiveEvents;

	bool EventSetup();
	bool EventCleanup();
    void CheckForVeto(bool &outWasVetoed);

	bool DoSimpleMouseEvent(const char *inType, wxPoint inPosition);

public:
	EventDispatcher();
	~EventDispatcher();

	//////////
	// Set the event-dispatching callback.
	//
	void SetDispatcher(TCallbackPtr inCallback);

	//////////
	// Notify the EventDispatcher that script is being reloaded.
	//
    void NotifyScriptReload();

	//////////
	// Turn "expensive" events--idle events, mouse moved events--on or
	// off.  These tend to generate a lot of garbage for the Scheme
	// GC to clean up.
	//
	static void EnableExpensiveEvents(bool inEnable);

    //////////
    // Dispatch an event asking for menus and buttons related to
    // inCommandName to be updated.  Similar to wxWindows EVT_UPDATE_UI.
    //
    bool DoEventUpdateUI(const wxString &inCommandName);

	//////////
	// Dispatch a mouse-down event.
	//
	bool DoEventLeftDown(wxMouseEvent &inEvent, bool inIsDoubleClick);

	///////////
	// Dispatch a mouse-up event.
	//
	bool DoEventLeftUp(wxMouseEvent &inEvent);

	//////////
	// Dispatch a mouse-enter event.
	//
	bool DoEventMouseEnter(wxPoint inPosition);

	//////////
	// Dispatch a mouse-leave event.
	//
	bool DoEventMouseLeave(wxPoint inPosition);

	//////////
	// Dispatch a character event.  Return true if the event was handled.
	//
	bool DoEventChar(wxKeyEvent &inEvent);

	//////////
	// Dispatch an idle event.
	//
	bool DoEventIdle(wxIdleEvent &inEvent);
	
	//////////
	// Dispatch a mouse move event.
	//
	bool DoEventMouseMoved(wxMouseEvent &inEvent);

    //////////
    // Dispatch a BrowserNavigate event.  This occurs before loading a new
    // page, and it may be vetoed.
    //
    bool DoEventBrowserNavigate(const wxString &inUrl, bool &outWasVetoed);

    //////////
    // Dispatch a PageChanged event.  This occurs whenever the browser
    // has loaded a new page, or accomplished some similar task.
    //
    bool DoEventBrowserPageChanged(const wxString &inUrl);

    //////////
    // Dispatch a TitleChanged event.  This occurs whenever the browser
    // window title changes.
    //
    bool DoEventBrowserTitleChanged(const wxString &inTitle);

    //////////
    // Dispatch a SetStatusText event.
    //
    bool DoEventStatusTextChanged(const wxString &inText);
    
    //////////
    // Dispatch a ProgressChanged event.
    //
    bool DoEventProgressChanged(bool inIsActive, double inPortionCompleted);

    //////////
    // Dispatch a MediaFinished event.
    //
    bool DoEventMediaFinished();
};

typedef shared_ptr<EventDispatcher> EventDispatcherPtr;

#endif // EventDispatcher_H
