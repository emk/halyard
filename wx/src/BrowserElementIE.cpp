// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include <wx/wfstream.h> // TODO - Probably temporary!
#include <IEHtmlWin.h>
#include "BrowserElementIE.h"
#include "EventDispatcher.h"


//=========================================================================
//  CustomIEHtmlWindow Support Class
//=========================================================================

class CustomIEHtmlWindow : public wxIEHtmlWin {
    BrowserElementIE *mElement;

public:
    CustomIEHtmlWindow(wxWindow *inParent,
                       const wxRect &inBounds,
                       BrowserElementIE *inElement);
};

CustomIEHtmlWindow::CustomIEHtmlWindow(wxWindow *inParent,
                                       const wxRect &inBounds,
                                       BrowserElementIE *inElement)
    : wxIEHtmlWin(inParent, -1,
                  inBounds.GetPosition(),
                  inBounds.GetSize(),
                  wxBORDER),
      mElement(inElement)
{
}


//=========================================================================
//  BrowserElementIE Methods
//=========================================================================

BrowserElementIE::BrowserElementIE(Stage *inStage, const wxString &inName,
                                   const wxRect &inBounds,
                                   FIVEL_NS TCallback *inDispatch)
    : BrowserElement(inStage, inName, inDispatch)
                      
{
    mHtmlWindow = new CustomIEHtmlWindow(inStage, inBounds, this);
    InitializeWidgetWindow(mHtmlWindow);
}

void BrowserElementIE::LoadFile(const wxString &inFileName){
    mHtmlWindow->LoadUrl(inFileName);
    // XXX - Should we convert to URLs instead of using streams?
    //wxFileInputStream input(inFileName);
    //if (!input.Ok())
    //    THROW("Error opening HTML file");
    //mHtmlWindow->LoadStream(&input);
}

void BrowserElementIE::LoadUrl(const wxString &inUrl){
    mHtmlWindow->LoadUrl(inUrl);
}

wxString BrowserElementIE::GetCurrentPageUrl() {
    // XXX - We can figure this out somehow.
    return "about:blank";
}

wxString BrowserElementIE::GetCurrentPageTitle() {
    // XXX - We can figure this out somehow.
    return "";
}

bool BrowserElementIE::CanGoBack() {
    // XXX - We can figure this out from the control update events.
    return true;
}

bool BrowserElementIE::GoBack() {
    return mHtmlWindow->GoBack();
}

bool BrowserElementIE::CanGoForward() {
    // XXX - We can figure this out from the control update events.
    return true;
}

bool BrowserElementIE::GoForward() {
    return mHtmlWindow->GoForward();
}

bool BrowserElementIE::CanRefresh() {
    return true;
}

bool BrowserElementIE::Refresh() {
    return mHtmlWindow->Refresh(wxIEHTML_REFRESH_NORMAL);
}

bool BrowserElementIE::CanStop() {
    // XXX - We can figure this out using various page load events.
    return true;
}

bool BrowserElementIE::Stop() {
    return mHtmlWindow->Stop();
}
