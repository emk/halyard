// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef EventDispatcher_H
#define EventDispatcher_H

#include <boost/utility.hpp>

class TCallback;

//////////
// This class passes events from our GUI to the interpreter.
//
class EventDispatcher : boost::noncopyable
{
    TCallback *mDispatcher;
	bool mEnableExpensiveEvents;

	bool EventSetup();
	bool EventCleanup();

public:
	EventDispatcher();
	~EventDispatcher();

	//////////
	// Set the event-dispatching callback.
	//
	void SetDispatcher(TCallback *inCallback);

	//////////
	// Notify the EventDispatcher that script is being reloaded.
	//
    void NotifyScriptReload();

	//////////
	// Turn "expensive" events--idle events, mouse moved events--on or
	// off.  These tend to generate a lot of garbage for the Scheme
	// GC to clean up.
	//
	void EnableExpensiveEvents(bool inEnable);

	typedef unsigned long Modifiers;
	enum /* Modifiers */ {
		Modifier_Control = 1,
		Modifier_Alt = 2,
		Modifier_Shift = 4
	};

	//////////
	// Dispatch a character event.  Return true if the event was handled.
	//
	bool DoEventChar(char inChar, Modifiers inModifiers);

	//////////
	// Dispatch an idle event.
	//
	bool DoEventIdle();
};

#endif // EventDispatcher_H
