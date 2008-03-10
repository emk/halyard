// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

#include <wx/laywin.h>
#include <wx/display.h>
#include <wx/config.h>
#include "SashFrame.h"

//=========================================================================
//  StageFrame Methods
//=========================================================================

BEGIN_EVENT_TABLE(SashFrame, wxFrame)
    EVT_SIZE(SashFrame::OnSize)
    EVT_SASH_DRAGGED(wxID_ANY, SashFrame::OnSashDrag)
END_EVENT_TABLE()

SashFrame::SashFrame(wxWindow *inParent,
                     wxWindowID inId,
                     const wxString &inTitle,
                     const wxString &inFrameName,
                     const wxSize &inSize,
                     long inStyle,
                     const wxString &inName)
    : wxFrame(inParent, inId, inTitle, LoadFramePosition(inFrameName),
              inSize, inStyle, inName),
      mHaveLoadedFrameLayout(false),
      mFrameName(inFrameName),
      mMainWindow(NULL)
{
    // Do nothing.
}

void SashFrame::SetMainWindow(wxWindow *inWindow) {
    mMainWindow = inWindow;
}

#if !wxUSE_DISPLAY

bool SashFrame::IsRectOnDisplay(const wxRect &inRect) {
    /// \TODO See if there's a way to get rough display bounds even if
    /// the required class is missing.
    return true;
}

#else // wxUSE_DISPLAY

bool SashFrame::IsRectOnDisplay(const wxRect &inRect) {
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

wxPoint SashFrame::LoadFramePosition(const wxString &inFrameName) {
    long pos_x, pos_y;
    wxConfigBase *config = wxConfigBase::Get();
    config->SetPath("/Layout/Default/" + inFrameName);
    if (config->Read("Left", &pos_x) && config->Read("Top", &pos_y) &&
        IsRectOnDisplay(wxRect(pos_x, pos_y, 100, 100)))
        return wxPoint(pos_x, pos_y);
    else
        return wxDefaultPosition;
}

wxConfigBase *SashFrame::GetConfigForFrame() {
    wxConfigBase *config = wxConfigBase::Get();
    config->SetPath("/Layout/Default/" + mFrameName);
    return config;
}

void SashFrame::LoadFrameLayout() {
    // Get our default values.
    wxSize sz = GetClientSize();
    long is_maximized = IsMaximized();
    long sz_client_width = sz.GetWidth();
    long sz_client_height = sz.GetHeight();
    
    // Load values from our config file.
    wxConfigBase *config = GetConfigForFrame();
    config->Read("IsMaximized", &is_maximized);
    config->Read("ClientWidth", &sz_client_width);
    config->Read("ClientHeight", &sz_client_height);
    
    // We restore our individual sash sizes before anything else, so they
    // will get appropriately adjusted by the frame resize events.
    LoadSashLayout(config);

    // Restore our non-maximized layout first.
    // NOTE - We'll only make the window larger, never smaller, because
    // we assume that GetClientSize is currently the minimum allowable.
    // This really only applies to StageFrame and not our other subclasses,
    // but it shouldn't hurt.
    wxSize new_size = GetClientSize();
    if (sz_client_width >= new_size.GetWidth() &&
        sz_client_height >= new_size.GetHeight())
        new_size = wxSize(sz_client_width, sz_client_height);
    SetClientSize(new_size);
    
    // If necessary, maximize our window.
    if (is_maximized)
        Maximize(TRUE);
    
    // It's now safe to resave these values.
    mHaveLoadedFrameLayout = true;
}

void SashFrame::MaybeSaveFrameLayout()
{
    // Don't save the frame layout if we haven't loaded it yet, or if we're
    // in full-screen mode (which has an automatically-chosen layout).
    if (!mHaveLoadedFrameLayout || IsFullScreen())
        return;
    
    wxConfigBase *config = GetConfigForFrame();
    config->Write("IsMaximized", IsMaximized() ? 1 : 0);
    if (!IsMaximized())
    {
        // Only save the window position if we aren't maximized.
        wxPoint pos = GetPosition();
        wxSize sz = GetClientSize();
        config->Write("Left", pos.x);
        config->Write("Top", pos.y);
        config->Write("ClientWidth", sz.GetWidth());
        config->Write("ClientHeight", sz.GetHeight());
    }
    SaveSashLayout(config);
}

void SashFrame::UpdateSashLayout() {
    // We may be called before a main window is set up, for example, when a
    // menu bar is added to the frame.
    if (mMainWindow) {
        // Ask wxLayoutAlgorithm to do smart resizing, taking our various
        // subwindows and constraints into account.
        wxLayoutAlgorithm layout;
        layout.LayoutFrame(this, mMainWindow);
        MaybeSaveFrameLayout();
    }
}

void SashFrame::OnSize(wxSizeEvent &inEvent) {
    UpdateSashLayout();
}

void SashFrame::OnSashDrag(wxSashEvent &inEvent)
{
    // Bail if the drag was invalid.
    if (inEvent.GetDragStatus() != wxSASH_STATUS_OK)
        return;

    // Figure out what sash was dragged.
    wxSashLayoutWindow *window =
        dynamic_cast<wxSashLayoutWindow *>(inEvent.GetEventObject());
    ASSERT(window);

    // Attempt to apply the drag to the window.
    //
    // TODO Does this work if we have a sash window anywhere but on
    // the left?
    window->SetDefaultSize(inEvent.GetDragRect().GetSize());
    UpdateSashLayout();
}
