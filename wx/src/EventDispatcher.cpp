// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TInterpreter.h"
#include "TVariable.h"
#include "EventDispatcher.h"
#include "TLogger.h"
 
BEGIN_NAMESPACE_FIVEL


//=========================================================================
// EventDispatcher Methods
//=========================================================================

EventDispatcher::EventDispatcher()
    : mDispatcher(NULL), mEnableExpensiveEvents(false)
{
}
 
EventDispatcher::~EventDispatcher()
{
    if (mDispatcher)
        delete mDispatcher;
}

void EventDispatcher::SetDispatcher(TCallback *inCallback)
{
    ASSERT(inCallback);
    if (mDispatcher)
        delete mDispatcher;
    mDispatcher = inCallback;
}

void EventDispatcher::NotifyScriptReload()
{
    if (mDispatcher)
        delete mDispatcher;
    mDispatcher = NULL;
}

void EventDispatcher::EnableExpensiveEvents(bool inEnable)
{
	if (inEnable == mEnableExpensiveEvents)
		return;
	if (inEnable)
		gDebugLog.Log("Turning expensive events on.");
	else
		gDebugLog.Log("Turning expensive events off.");
	mEnableExpensiveEvents = inEnable;
}

bool EventDispatcher::EventSetup()
{
	if (!mDispatcher)
		return false;

    // Clear our "pass" flag.
    gVariableManager.SetBoolean("_pass", false);
	return true;
}

bool EventDispatcher::EventCleanup()
{
    // Check our "pass" flag.
    return !gVariableManager.GetBoolean("_pass");
}

bool EventDispatcher::DoEventLeftDown(wxMouseEvent &inEvent,
									  bool inIsDoubleClick)
{
	if (!EventSetup())
		return false;

	std::auto_ptr<TCallbackArgumentList> args(mDispatcher->MakeArgumentList());
    args->AddSymbolArg("mouse-down");
	args->AddInt32Arg(inEvent.GetPosition().x);
	args->AddInt32Arg(inEvent.GetPosition().y);
	args->AddBoolArg(inIsDoubleClick);
    mDispatcher->Run(args.get());

	return EventCleanup();
}

bool EventDispatcher::DoSimpleMouseEvent(const char *inType,
										 wxPoint inPosition)
{
	if (!EventSetup())
		return false;

	std::auto_ptr<TCallbackArgumentList> args(mDispatcher->MakeArgumentList());
    args->AddSymbolArg(inType);
	args->AddInt32Arg(inPosition.x);
	args->AddInt32Arg(inPosition.y);
    mDispatcher->Run(args.get());

	return EventCleanup();
}

bool EventDispatcher::DoEventLeftUp(wxMouseEvent &inEvent)
{
	return DoSimpleMouseEvent("mouse-up", inEvent.GetPosition());
}

bool EventDispatcher::DoEventMouseEnter(wxPoint inPosition)
{
	return DoSimpleMouseEvent("mouse-enter", inPosition);
}

bool EventDispatcher::DoEventMouseLeave(wxPoint inPosition)
{
	return DoSimpleMouseEvent("mouse-leave", inPosition);
}

bool EventDispatcher::DoEventChar(wxKeyEvent &inEvent)
{
    if (!EventSetup())
		return false;

    // Turn our character into a string.
    char str[2];
    str[0] = inEvent.GetKeyCode();
    str[1] = '\0';

    // Build an argument list and call our dispatcher.
	std::auto_ptr<TCallbackArgumentList> args(mDispatcher->MakeArgumentList());
    args->AddSymbolArg("char");
    args->AddStringArg(str);
    args->BeginListArg();
    if (inEvent.ControlDown())
        args->AddSymbolArg("control");
    if (inEvent.AltDown())
        args->AddSymbolArg("alt");
    if (inEvent.ShiftDown())
        args->AddSymbolArg("shift");
    args->EndListArg();
    mDispatcher->Run(args.get());

	return EventCleanup();
}

bool EventDispatcher::DoEventIdle(wxIdleEvent &inEvent)
{
	if (!mEnableExpensiveEvents)
		return false;

    if (!EventSetup())
		return false;

	std::auto_ptr<TCallbackArgumentList> args(mDispatcher->MakeArgumentList());
    args->AddSymbolArg("idle");
    mDispatcher->Run(args.get());

	return EventCleanup();	
}

