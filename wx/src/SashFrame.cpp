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

#include <wx/laywin.h>
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
                     const wxPoint &inPos,
                     const wxSize &inSize,
                     long inStyle,
                     const wxString &inName)
    : wxFrame(inParent, inId, inTitle, inPos, inSize, inStyle, inName),
      mMainWindow(NULL)
{
    // Do nothing.
}

void SashFrame::SetMainWindow(wxWindow *inWindow) {
    mMainWindow = inWindow;
}

void SashFrame::UpdateSashLayout() {
    // We may be called before a main window is set up, for example, when a
    // menu bar is added to the frame.
    if (mMainWindow) {
	// Ask wxLayoutAlgorithm to do smart resizing, taking our various
	// subwindows and constraints into account.
        wxLayoutAlgorithm layout;
        layout.LayoutFrame(this, mMainWindow);
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
