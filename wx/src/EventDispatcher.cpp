// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include "TInterpreter.h"
#include "EventDispatcher.h"
 
BEGIN_NAMESPACE_FIVEL


//=========================================================================
// EventDispatcher Methods
//=========================================================================

bool EventDispatcher::sEnableExpensiveEvents = false;

EventDispatcher::EventDispatcher()
{
}
 
EventDispatcher::~EventDispatcher()
{
}

void EventDispatcher::SetDispatcher(TCallbackPtr inCallback)
{
    ASSERT(inCallback.get());
    mDispatcher = inCallback;
}

void EventDispatcher::NotifyScriptReload()
{
    mDispatcher.reset();
	sEnableExpensiveEvents = false;
}

void EventDispatcher::EnableExpensiveEvents(bool inEnable)
{
	if (inEnable == sEnableExpensiveEvents)
		return;
	if (inEnable)
		gDebugLog.Log("Turning expensive events on.");
	else
		gDebugLog.Log("Turning expensive events off.");
	sEnableExpensiveEvents = inEnable;
}

bool EventDispatcher::EventSetup()
{
	if (!mDispatcher)
		return false;

    // Clear our "pass" and "vetoed" flags.
    gVariableManager.SetBoolean("_pass", false);
    gVariableManager.SetBoolean("_veto", false);
	return true;
}

bool EventDispatcher::EventCleanup()
{
    // Check our "pass" flag.
    return !gVariableManager.GetBoolean("_pass");
}

void EventDispatcher::CheckForVeto(bool &outWasVetoed) {
    // See if this event was vetoed.
    outWasVetoed = (!gVariableManager.GetBoolean("_pass") &&
					gVariableManager.GetBoolean("_veto"));
}

bool EventDispatcher::DoEventUpdateUI(const wxString &inCommandName) {
	if (!EventSetup())
		return false;

	TValueList args;
	args.push_back(TSymbol("update-ui"));
	args.push_back(TSymbol(inCommandName.mb_str()));
	mDispatcher->Run(args);
	return EventCleanup();    
}

bool EventDispatcher::DoEventLeftDown(wxMouseEvent &inEvent,
									  bool inIsDoubleClick)
{
	if (!EventSetup())
		return false;

	TValueList args;
	args.push_back(TSymbol("mouse-down"));
	args.push_back(inEvent.GetPosition().x);
	args.push_back(inEvent.GetPosition().y);
	args.push_back(inIsDoubleClick);
    mDispatcher->Run(args);
	return EventCleanup();
}

bool EventDispatcher::DoSimpleMouseEvent(const char *inType,
										 wxPoint inPosition)
{
	if (!EventSetup())
		return false;

	TValueList args;
	args.push_back(TSymbol(inType));
	args.push_back(inPosition.x);
	args.push_back(inPosition.y);
	mDispatcher->Run(args);
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

	TValueList args, modifiers;
    args.push_back(TSymbol("char"));
    args.push_back(str);

    if (inEvent.ControlDown())
        modifiers.push_back(TSymbol("control"));
    if (inEvent.AltDown())
        modifiers.push_back(TSymbol("alt"));
    if (inEvent.ShiftDown())
        modifiers.push_back(TSymbol("shift"));

	args.push_back(modifiers);

    mDispatcher->Run(args);
	return EventCleanup();
}

bool EventDispatcher::DoEventIdle(wxIdleEvent &inEvent)
{
	if (!sEnableExpensiveEvents)
		return false;

    if (!EventSetup())
		return false;

	TValueList args;
    args.push_back(TSymbol("idle"));
    mDispatcher->Run(args);
	return EventCleanup();
}

bool EventDispatcher::DoEventMouseMoved(wxMouseEvent &inEvent)
{
	if (!sEnableExpensiveEvents)
		return false;
	
	return DoSimpleMouseEvent("mouse-moved", inEvent.GetPosition());
}
	
bool EventDispatcher::DoEventBrowserNavigate(const wxString &inUrl,
                                             bool &outWasVetoed)
{
    if (!EventSetup())
		return false;

	TValueList args;
    args.push_back(TSymbol("browser-navigate"));
    args.push_back(inUrl.mb_str());
    mDispatcher->Run(args);
    CheckForVeto(outWasVetoed);
	return EventCleanup();
}

bool EventDispatcher::DoEventBrowserPageChanged(const wxString &inUrl) {
    if (!EventSetup())
		return false;

	TValueList args;
    args.push_back(TSymbol("browser-page-changed"));
    args.push_back(inUrl.mb_str());
    mDispatcher->Run(args);
	return EventCleanup();
}

bool EventDispatcher::DoEventBrowserTitleChanged(const wxString &inTitle) {
    if (!EventSetup())
		return false;

	TValueList args;
    args.push_back(TSymbol("browser-title-changed"));
    args.push_back(inTitle.mb_str());
    mDispatcher->Run(args);
	return EventCleanup();
}

bool EventDispatcher::DoEventStatusTextChanged(const wxString &inText) {
    if (!EventSetup())
		return false;

	TValueList args;
    args.push_back(TSymbol("status-text-changed"));
    args.push_back(inText.mb_str());
    mDispatcher->Run(args);
	return EventCleanup();
}

bool EventDispatcher::DoEventProgressChanged(bool inIsActive,
                                             double inPortionCompleted)
{
    if (!EventSetup())
		return false;

	TValueList args;
    args.push_back(TSymbol("progress-changed"));
    args.push_back(inIsActive);
    args.push_back(inPortionCompleted);
    mDispatcher->Run(args);
	return EventCleanup();
}
