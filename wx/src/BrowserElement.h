// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef BrowserElement_H
#define BrowserElement_H

#include "Widget.h"
#include "TInterpreter.h"

class EventDispatcher;

//////////
// A browser element displays a web browser of some sort.
//
class BrowserElement : public Widget {
    EventDispatcher *mDispatcher;

protected:
    //////////
    // Create a new browser element with the specified event handlers.
    //
    // [in] inStage - The stage on which to create the element.
    // [in] inName - The element name.
    // [in] inDispatch - The event dispatcher to use.
    //
    BrowserElement(Stage *inStage, const wxString &inName,
                   FIVEL_NS TCallbackPtr inDispatch);

    //////////
    // You'll need to dispatch a few events to this dispatcher, typically
    // DoEventBrowserNavigate, DoEventBrowserPageChanged,
    // DoEventStatusTextChanged and DoEventProgressChanged.
    //
    EventDispatcher *GetDispatcher() { return mDispatcher; }

public:
	virtual ~BrowserElement();

    //////////
    // Open the specified URL in the browser window.
    //
    virtual void LoadPage(const wxString &inUrl) = 0;

    //////////
    // Get the URL of the currently-opened page.  In a perfect world,
    // this would return "about:blank" if no page is open.
    //
    virtual wxString GetCurrentPageUrl() = 0;

    //////////
    // Get the title of the currently-opened page.
    //
    virtual wxString GetCurrentPageTitle() = 0;

    //////////
    // Returns true if the back button should be enabled.
    //
    virtual bool CanGoBack() = 0;

    //////////
    // Try to go back.  Returns true if it succeeds.
    //
    virtual bool GoBack() = 0;

    //////////
    // Returns true if the back button should be enabled.
    //
    virtual bool CanGoForward() = 0;

    //////////
    // Try to go foward.  Returns true if it succeeds.
    //
    virtual bool GoForward() = 0;

    //////////
    // Returns true if the refresh button should be enabled.
    //
    virtual bool CanRefresh() = 0;

    //////////
    // Try to refresh the page.  Returns true if it succeeds.
    //
    virtual bool Refresh() = 0;

    //////////
    // Returns true if the stop button should be enabled.
    // XXX - Do we need an event to notify us of control updates?
    //
    virtual bool CanStop() = 0;

    //////////
    // Try to stop any active page loads.  Returns true if it succeeds.
    //
    virtual bool Stop() = 0;
};

#endif // BrowserElement_H
