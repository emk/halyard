// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef BrowserElementWx_H
#define BrowserElementWx_H

#include "BrowserElement.h"
#include "TInterpreter.h"


//////////
// A simple browser based on the wxWindows help viewer.
//
class BrowserElementWx : public BrowserElement {
    friend class CustomWxHtmlWindow;

    CustomWxHtmlWindow *mHtmlWindow;

public:
    BrowserElementWx(Stage *inStage, const wxString &inName,
                     const wxRect &inBounds,
                     FIVEL_NS TCallback *inDispatch);

    virtual void LoadPage(const wxString &inUrl);
    virtual wxString GetCurrentPageUrl();
    virtual wxString GetCurrentPageTitle();
    virtual bool CanGoBack();
    virtual bool GoBack();
    virtual bool CanGoForward();
    virtual bool GoForward();
    virtual bool CanRefresh();
    virtual bool Refresh();
    virtual bool CanStop();
    virtual bool Stop();
};

#endif // BrowserElementWx_H
