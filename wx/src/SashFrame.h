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

#ifndef SashFrame_H
#define SashFrame_H

class wxSashEvent;

/// A regular window frame with some helpful support code for sash windows.
class SashFrame : public wxFrame {
    /// The main subwindow of this frame.
    wxWindow *mMainWindow;

public:
    /// Create a new SashFrame.  Parameters are the same as wxFrame.
    SashFrame(wxWindow *inParent,
              wxWindowID inId,
              const wxString &inTitle,
              const wxPoint &inPos = wxDefaultPosition,
              const wxSize &inSize = wxDefaultSize,
              long inStyle = wxDEFAULT_FRAME_STYLE,
              const wxString &inName = wxFrameNameStr);

protected:
    /// Specify which wxSashLayoutWindow should be treated as the
    /// main window of this frame.
    void SetMainWindow(wxWindow *inWindow);

    /// Lay out the window.
    virtual void UpdateSashLayout();

private:
    //////////
	/// "Sashes" are narrow bars between subwindows in frame.  When
	/// a sash in the main window is dragged, it generates an event
	/// which we process here.
	///
	void OnSashDrag(wxSashEvent &inEvent);

    //////////
	/// We provide an OnSize handler so we can handle window resizing
	/// gracefully.
	///
	void OnSize(wxSizeEvent &inEvent);    

    DECLARE_EVENT_TABLE();
};

#endif // SashFrame_H
