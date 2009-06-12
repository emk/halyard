// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#ifndef BrowserElement_H
#define BrowserElement_H

#include "Widget.h"
#include "TInterpreter.h"

//////////
/// A browser element displays a web browser of some sort.
///
class BrowserElement : public Widget {
protected:
    /// Create a new browser element with the specified event handlers.
    ///
    /// \param inStage  The stage on which to create the element.
    /// \param inName  The element name.
    /// \param inDispatch  The event dispatcher to use.
    BrowserElement(Stage *inStage, const wxString &inName,
                   Halyard::TCallbackPtr inDispatch);

public:
    /// Open the specified URL in the browser window.
    virtual void LoadPage(const wxString &inUrl) = 0;

    /// Get the URL of the currently-opened page.  In a perfect world,
    /// this would return "about:blank" if no page is open.
    virtual wxString GetCurrentPageUrl() = 0;

    /// Get the title of the currently-opened page.
    virtual wxString GetCurrentPageTitle() = 0;

    /// Returns true if the back button should be enabled.
    virtual bool CanGoBack() = 0;

    /// Try to go back.  Returns true if it succeeds.
    virtual bool GoBack() = 0;

    /// Returns true if the back button should be enabled.
    virtual bool CanGoForward() = 0;

    /// Try to go foward.  Returns true if it succeeds.
    virtual bool GoForward() = 0;

    /// Returns true if the refresh button should be enabled.
    virtual bool CanRefresh() = 0;

    /// Try to refresh the page.  Returns true if it succeeds.
    virtual bool Refresh() = 0;

    /// Returns true if the stop button should be enabled.
    /// XXX - Do we need an event to notify us of control updates?
    virtual bool CanStop() = 0;

    /// Try to stop any active page loads.  Returns true if it succeeds.
    virtual bool Stop() = 0;
};

// Define BrowserElementNative to map to an appropriate browser clas.  This
// is essentially a low-budget "factory" pattern.
#include "BrowserElementWx.h"
#if defined __WXMSW__
#   include "BrowserElementIE.h"
#   define BrowserElementNative BrowserElementIE
#else // !defined __WXMSW__
#   define BrowserElementNative BrowserElementWx
#endif // !defined __WXMSW__

#endif // BrowserElement_H
