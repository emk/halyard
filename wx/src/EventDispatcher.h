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
};

#endif // EventDispatcher_H
