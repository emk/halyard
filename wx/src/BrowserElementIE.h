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

#ifndef BrowserElementIE_H
#define BrowserElementIE_H

#include "BrowserElement.h"
#include "TInterpreter.h"


//////////
/// A browser element based on embedding Internet Explorer via ActiveX.  This
/// could be trivially generalized to support any other browser controls
/// which implement IE's ActiveX interfaces.
///
/// PORTING - This obviously isn't going to work anywhere but Windows.
///
class BrowserElementIE : public BrowserElement {
    friend class CustomIEHtmlWindow;

    enum {
        BACK_BUTTON,
        FORWARD_BUTTON,
        MAX_BUTTONS
    };
    typedef long BrowserControlFlags;

    CustomIEHtmlWindow *mHtmlWindow;
    bool mButtonEnabled[MAX_BUTTONS];

public:
    BrowserElementIE(Stage *inStage, const wxString &inName,
                     const wxRect &inBounds,
                     FIVEL_NS TCallbackPtr inDispatch);

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

#endif // BrowserElementIE_H
