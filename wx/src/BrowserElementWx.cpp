// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include <wx/html/htmlwin.h>
#include "BrowserElementWx.h"
#include "EventDispatcher.h"


//=========================================================================
//  CustomWxHtmlWindow Support Class
//=========================================================================

class CustomWxHtmlWindow : public wxHtmlWindow {
    BrowserElementWx *mElement;

public:
    CustomWxHtmlWindow(wxWindow *inParent,
                       const wxRect &inBounds,
                       BrowserElementWx *inElement);
    virtual bool LoadPage(const wxString& inLocation);
    virtual wxHtmlOpeningStatus OnOpeningURL(wxHtmlURLType inType,
                                             const wxString& inUrl,
                                             wxString *outRedirect) const;
};

CustomWxHtmlWindow::CustomWxHtmlWindow(wxWindow *inParent,
                                       const wxRect &inBounds,
                                       BrowserElementWx *inElement)
    : wxHtmlWindow(inParent, -1,
                   inBounds.GetPosition(),
                   inBounds.GetSize(),
                   wxHW_SCROLLBAR_AUTO | wxBORDER),
	  mElement(inElement)
{
}

bool CustomWxHtmlWindow::LoadPage(const wxString& inLocation) {
    bool loaded = wxHtmlWindow::LoadPage(inLocation);
    if (loaded) {
        // We can't use inLocation here, because it may be relative.
        EventDispatcher *dispatcher = mElement->GetDispatcher();
        dispatcher->DoEventBrowserPageChanged(GetOpenedPage());
        dispatcher->DoEventBrowserTitleChanged(GetOpenedPageTitle());
        dispatcher->DoEventUpdateUI("back");
        dispatcher->DoEventUpdateUI("forward");
    }
    return loaded;
}

wxHtmlOpeningStatus
CustomWxHtmlWindow::OnOpeningURL(wxHtmlURLType inType,
                                 const wxString& inUrl,
                                 wxString *outRedirect) const
{
    // See if the scripting language wants to veto this URL.
    bool vetoed = false;
    if (inType == wxHTML_URL_PAGE) {
        BrowserElementWx *elem = const_cast<BrowserElementWx*>(mElement);
        elem->GetDispatcher()->DoEventBrowserNavigate(inUrl, vetoed);
    }
    if (vetoed)
        return wxHTML_BLOCK;
    else
        return wxHtmlWindow::OnOpeningURL(inType, inUrl, outRedirect);
}


//=========================================================================
//  BrowserElementWx Methods
//=========================================================================

BrowserElementWx::BrowserElementWx(Stage *inStage, const wxString &inName,
                                   const wxRect &inBounds,
                                   FIVEL_NS TCallback *inDispatch)
    : BrowserElement(inStage, inName, inDispatch)
                      
{
    mHtmlWindow = new CustomWxHtmlWindow(inStage, inBounds, this);
    InitializeWidgetWindow(mHtmlWindow);
}

void BrowserElementWx::LoadPage(const wxString &inUrl){
    mHtmlWindow->LoadPage(inUrl);
}

wxString BrowserElementWx::GetCurrentPageUrl() {
    wxString url(mHtmlWindow->GetOpenedPage());
    if (url == "")
        return "about:blank";
    else
        return url;
}

wxString BrowserElementWx::GetCurrentPageTitle() {
    return mHtmlWindow->GetOpenedPageTitle();
}

bool BrowserElementWx::CanGoBack() {
    return mHtmlWindow->HistoryCanBack();
}

bool BrowserElementWx::GoBack() {
    return mHtmlWindow->HistoryBack();
}

bool BrowserElementWx::CanGoForward() {
    return mHtmlWindow->HistoryCanForward();
}

bool BrowserElementWx::GoForward() {
    return mHtmlWindow->HistoryForward();
}

bool BrowserElementWx::CanRefresh() {
    return false;
}

bool BrowserElementWx::Refresh() {
    return false;
}

bool BrowserElementWx::CanStop() {
    return false;
}

bool BrowserElementWx::Stop() {
    return false;
}
