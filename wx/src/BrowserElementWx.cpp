// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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
        EventDispatcherPtr dispatcher = mElement->GetEventDispatcher();
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
        elem->GetEventDispatcher()->DoEventBrowserNavigate(inUrl, vetoed);
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
                                   FIVEL_NS TCallbackPtr inDispatch)
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
