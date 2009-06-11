// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

#include "AppHeaders.h"

#include <wx/aui/framemanager.h>
#include <wx/display.h>
#include <wx/config.h>
#include "AuiFrame.h"

//=========================================================================
//  StageFrame Methods
//=========================================================================

BEGIN_EVENT_TABLE(AuiFrame, wxFrame)
END_EVENT_TABLE()

AuiFrame::AuiFrame(wxWindow *inParent,
                   wxWindowID inId,
                   const wxString &inTitle,
                   const wxString &inFrameName,
                   const wxSize &inSize,
                   long inStyle,
                   const wxString &inName)
  : wxFrame(inParent, inId, inTitle, LoadFramePosition(inFrameName),
            inSize, inStyle, inName),
    mHaveLoadedFramePerspective(false),
    mFrameName(inFrameName)
{
    // Create our wxAuiManager.
    mAuiManager.reset(new wxAuiManager(this));
}

#if !wxUSE_DISPLAY

bool AuiFrame::IsRectOnDisplay(const wxRect &inRect) {
    /// \TODO See if there's a way to get rough display bounds even if
    /// the required class is missing.
    return true;
}

#else // wxUSE_DISPLAY

bool AuiFrame::IsRectOnDisplay(const wxRect &inRect) {
    size_t count = wxDisplay::GetCount();
    for (size_t i = 0; i < count; i++) {
        wxRect display(wxDisplay(i).GetGeometry());
        if (display.GetLeft() <= inRect.GetLeft()
            && inRect.GetRight() <= display.GetRight()
            && display.GetTop() <= inRect.GetTop()
            && inRect.GetBottom() <= display.GetBottom())
            return true;
    }
    return false;
}

#endif // wxUSE_DISPLAY

wxPoint AuiFrame::LoadFramePosition(const wxString &inFrameName) {
    long pos_x, pos_y;
    shared_ptr<wxConfigBase> config(new wxConfig);
    config->SetPath(wxT("/Perspectives/") + inFrameName);
    if (config->Read(wxT("Left"), &pos_x) && config->Read(wxT("Top"), &pos_y) &&
        IsRectOnDisplay(wxRect(pos_x, pos_y, 100, 100)))
        return wxPoint(pos_x, pos_y);
    else
        return wxDefaultPosition;
}

shared_ptr<wxConfigBase> AuiFrame::GetConfigForFrame() {
    shared_ptr<wxConfigBase> config(new wxConfig);
    config->SetPath(wxT("/Perspectives/") + mFrameName);
    return config;
}

void AuiFrame::LoadFramePerspective() {
    // Load our window size.
    shared_ptr<wxConfigBase> config(GetConfigForFrame());
    long sz_client_width = GetClientSize().GetWidth();
    long sz_client_height = GetClientSize().GetHeight();
    config->Read(wxT("ClientWidth"), &sz_client_width);
    config->Read(wxT("ClientHeight"), &sz_client_height);
    SetClientSize(sz_client_width, sz_client_height);

    // Load our perspective.
    wxString perspective;
    if (config->Read(wxT("Perspective"), &perspective))
        mAuiManager->LoadPerspective(perspective);

    // Load our maximized state after every else has been set up.
    long is_maximized = IsMaximized();
    config->Read(wxT("IsMaximized"), &is_maximized);
    if (is_maximized)
        Maximize(true);
    
    // It's now safe to resave these values.
    mHaveLoadedFramePerspective = true;
}

void AuiFrame::MaybeSaveFramePerspective()
{
    // Don't save the frame perspective if we haven't loaded it yet, or if
    // we're in full-screen mode (which has an automatically-chosen
    // perspective).
    if (!mHaveLoadedFramePerspective || IsFullScreen())
        return;

    // Write our perspective to disk.
    shared_ptr<wxConfigBase> config(GetConfigForFrame());
    config->Write(wxT("Perspective"),
                  mAuiManager->SavePerspective());
    
    // Write our other values to disk.
    config->Write(wxT("IsMaximized"), IsMaximized() ? 1 : 0);
    if (!IsMaximized()) {
        // Only save the window position if we aren't maximized.
        wxPoint pos = GetPosition();
        wxSize sz = GetClientSize();
        config->Write(wxT("Left"), pos.x);
        config->Write(wxT("Top"), pos.y);
        config->Write(wxT("ClientWidth"), sz.GetWidth());
        config->Write(wxT("ClientHeight"), sz.GetHeight());
    }
}

bool AuiFrame::Layout() {
    // After wxAui has finished showing and hiding panes, applying sizers,
    // and setting up docks, it calls Layout() to run the sizer code.  But
    // wxAui doesn't implement minimum frame sizes correctly, so we need to
    // intercept this layout call and try to make sure our frame is big
    // enough.
    bool result = wxFrame::Layout();
    UpdateMinimumFrameSize();
    return result;
}

void AuiFrame::UpdateMinimumFrameSize() {
    // The code in this function is loosely based on a commented-out
    // snippet at the end of wxAuiManager::Update.  The original code was
    // distributed under the wxWindows Library Licence, Version 3.1, and
    // copyright 2005-2006 by Kirix Corporation, but we've changed it quite
    // a bit.  And yes, this code is probably fairly fragile if anyone does
    // serious work on wxAui.

    // Ask our sizer how much space we need for the contents of our frame.
    // If we don't have a sizer yet, then we're being called before
    // wxAuiManager::Update has been called for the first time, so we just
    // do nothing.
    wxSizer *sizer(GetSizer());
    if (!sizer)
        return;
    wxSize min_content_size(sizer->GetMinSize());

    // Figure out how much space we need for menu bars, status bars, and
    // other non-"client" stuff around our frame.
    wxSize frame_size(GetSize());
    wxSize min_frame_size(min_content_size + (frame_size - GetClientSize()));

    // Set our minimum size, and possibly update our real size.
    SetMinSize(min_frame_size);
    if (frame_size.x < min_frame_size.x || frame_size.y < min_frame_size.y) {
        wxSize fitting_size(sizer->ComputeFittingWindowSize(this));
        SetSize(std::max(frame_size.x, fitting_size.x),
                std::max(frame_size.y, fitting_size.y));
    }
}

