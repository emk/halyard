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

#ifndef SashFrame_H
#define SashFrame_H

class wxSashEvent;
class wxConfigBase;

/// A regular window frame with some helpful support code for sash windows.
class SashFrame : public wxFrame {
    bool mHaveLoadedFrameLayout;

    /// The name of this frame (used to store wxConfig data).
    wxString mFrameName;

    /// The main subwindow of this frame.
    wxWindow *mMainWindow;

public:
    /// Create a new SashFrame.  Parameters are the same as wxFrame.
    SashFrame(wxWindow *inParent,
              wxWindowID inId,
              const wxString &inTitle,
              const wxString &inFrameName,
              const wxSize &inSize = wxDefaultSize,
              long inStyle = wxDEFAULT_FRAME_STYLE,
              const wxString &inName = wxFrameNameStr);

private:
    /// Returns true iff inRect is entirely on one of the system's
    /// displays.  Used for trying to put windows in the same place they
    /// were the last time they were closed, but only if that's a
    /// reasonably sane thing to do.
    static bool IsRectOnDisplay(const wxRect &inRect);

	/// We need to load this layout information *before* we load anything
	/// else, because there's no portable way to change it once the window
	/// is created.
	static wxPoint LoadFramePosition(const wxString &inFrameName);

protected:
    /// Get a configuration object with its path set to the appropriate
    /// value for this frame.
    wxConfigBase *GetConfigForFrame();

	/// Load the layout for the current frame.
	void LoadFrameLayout();

    /// Load the layout information for any sash windows.
    virtual void LoadSashLayout(wxConfigBase *inConfig) {}

	/// Save the layout for the current frame if it's safe to do so.
	void MaybeSaveFrameLayout();

    /// Save the layout information for any sash windows.
    virtual void SaveSashLayout(wxConfigBase *inConfig) {}

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
