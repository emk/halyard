// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#include "TamaleHeaders.h"

#include "TInterpreter.h"
#include "EventDispatcher.h"
 
USING_NAMESPACE_FIVEL


//=========================================================================
// EventDispatcher Methods
//=========================================================================

bool EventDispatcher::sEnableExpensiveEvents = false;
bool EventDispatcher::sMaxStaleTimeInitialized = false;
wxLongLong EventDispatcher::sMaxStaleTime = 0;

EventDispatcher::EventDispatcher() {
    // One-time initialization of sMaxStaleTime.
    if (!sMaxStaleTimeInitialized) {
        sMaxStaleTimeInitialized = true;
        UpdateMaxStaleTime();
    }
}
 
EventDispatcher::~EventDispatcher()
{
}

bool EventDispatcher::IsEventStale(const wxEvent &event) {
    // TODO - This function should probably be merged into EventSetup and
    // modified to apply to all event types. But we don't have wxEvent
    // objects for all our events yet, so there's no easy way to make this
    // consistent. Oh, well.
    return PlatformGetEventTimestamp(event) <= sMaxStaleTime;
}

void EventDispatcher::UpdateMaxStaleTime() {
    sMaxStaleTime = PlatformGetTickCount();    
}

void EventDispatcher::SetDispatcher(TCallbackPtr inCallback)
{
    ASSERT(inCallback.get());
    mDispatcher = inCallback;
}

void EventDispatcher::NotifyReloadScriptStarting()
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
    gVariableManager.Set("_pass", false);
    gVariableManager.Set("_veto", false);
	return true;
}

bool EventDispatcher::EventCleanup()
{
    // Any as-yet-unprocessed events which occurred before this time are
    // considered "stale", and may be ignored if the script so desires.
    UpdateMaxStaleTime();

    // Check our "pass" flag.
    return !bool(gVariableManager.Get("_pass"));
}

void EventDispatcher::CheckForVeto(bool &outWasVetoed) {
    // See if this event was vetoed.
    outWasVetoed = (!bool(gVariableManager.Get("_pass")) &&
					bool(gVariableManager.Get("_veto")));
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
    args.push_back(IsEventStale(inEvent));
    mDispatcher->Run(args);
	return EventCleanup();
}

bool EventDispatcher::DoSimpleEvent(const char *inType) {
    if (!EventSetup())
		return false;

	TValueList args;
    args.push_back(TSymbol(inType));
    mDispatcher->Run(args);
	return EventCleanup();
}

bool EventDispatcher::DoSimpleMouseEvent(const char *inType,
										 wxPoint inPosition,
                                         bool inIsStale)
{
	if (!EventSetup())
		return false;

	TValueList args;
	args.push_back(TSymbol(inType));
	args.push_back(inPosition.x);
	args.push_back(inPosition.y);
    args.push_back(inIsStale);
	mDispatcher->Run(args);
	return EventCleanup();
}

bool EventDispatcher::DoEventLeftUp(wxMouseEvent &inEvent)
{
	return DoSimpleMouseEvent("mouse-up", inEvent.GetPosition(),
                              IsEventStale(inEvent));
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
    args.push_back(IsEventStale(inEvent));

    mDispatcher->Run(args);
	return EventCleanup();
}

bool EventDispatcher::DoEventIdle()
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

bool EventDispatcher::DoEventMediaFinished() {
    return DoSimpleEvent("media-finished");
}

bool EventDispatcher::DoEventMediaLocalError() {
    return DoSimpleEvent("media-local-error");
}

bool EventDispatcher::DoEventMediaNetworkError() {
    return DoSimpleEvent("media-network-error");
}

bool EventDispatcher::DoEventMediaNetworkTimeout() {
    return DoSimpleEvent("media-network-timeout");
}

bool EventDispatcher::DoEventMediaCaption(const std::string &caption) {
    if (!EventSetup())
        return false;
    
	TValueList args;
    args.push_back(TSymbol("media-caption"));
    args.push_back(caption);
    mDispatcher->Run(args);
    
	return EventCleanup();    
}



//=========================================================================
// Platform-Specific Methods
//=========================================================================

#ifdef FIVEL_PLATFORM_WIN32

#include <windows.h>

wxLongLong EventDispatcher::PlatformGetEventTimestamp(const wxEvent &event) {
    // XXX - We need to cast to a DWORD first, because the Win32 API (and
    // wxWidgets) treat the return value of GetMessageTime as a 'LONG'
    // instead of a 'DWORD'.
    ASSERT(event.GetTimestamp() == ::GetMessageTime());
    return (wxLongLong) ((DWORD) event.GetTimestamp());
}

wxLongLong EventDispatcher::PlatformGetTickCount() {
    // The wxWidgets function wxEvent::GetTimestamp is implemented 
    // with ::GetMessageTime(), which returns the time in the same
    // units as ::GetTickCount(), according to the ::GetTickCount()
    // MSDN documentation.
    //
    // A similar hack will be required on other platforms as well.
    return ::GetTickCount();
}

#endif
