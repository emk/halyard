// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TCommon.h"
#include "TInterpreter.h"
#include "TVariable.h"
#include "EventDispatcher.h"
 
BEGIN_NAMESPACE_FIVEL


//=========================================================================
// EventDispatcher Methods
//=========================================================================

EventDispatcher::EventDispatcher()
    : mDispatcher(NULL)
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

bool EventDispatcher::DoEventChar(char inChar, Modifiers inModifiers)
{
    if (!mDispatcher)
		return false;

    // Clear our "pass" flag.
    gVariableManager.SetBoolean("_pass", false);

    // Turn our character into a string.
    char str[2];
    str[0] = inChar;
    str[1] = '\0';

    // Build an argument list and call our dispatcher.
    mDispatcher->BeginArguments();
    mDispatcher->AddSymbolArg("char");
    mDispatcher->AddStringArg(str);
    mDispatcher->BeginListArg();
    if (inModifiers & Modifier_Control)
        mDispatcher->AddSymbolArg("control");
    if (inModifiers & Modifier_Alt)
        mDispatcher->AddSymbolArg("alt");
    if (inModifiers & Modifier_Shift)
        mDispatcher->AddSymbolArg("shift");
    mDispatcher->EndListArg();
    mDispatcher->EndArguments();
    mDispatcher->Run();

    // Check our "pass" flag.
    return !gVariableManager.GetBoolean("_pass");
}