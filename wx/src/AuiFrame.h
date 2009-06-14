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

#ifndef AuiFrame_H
#define AuiFrame_H

class wxConfigBase;
class wxAuiManager;

/// A regular window frame with some helpful support code for sash windows.
class AuiFrame : public wxFrame {
    DECLARE_EVENT_TABLE();

    /// Our wxAuiManager is in charge of laying out our window.
    scoped_ptr<wxAuiManager> mAuiManager;

    /// Have we loaded the perspective for this frame yet?
    bool mHaveLoadedFramePerspective;

    /// The name of this frame (used to store wxConfig data).
    wxString mFrameName;

    /// The origin perspective we used when setting up this frame.  This
    /// allows users to reset their perspective back to the default.
    wxString mDefaultPerspective;

public:
    /// Create a new AuiFrame.  Parameters are the same as wxFrame, except
    /// for inFrameName, which is used to keep track of which saved
    /// perspectives belong to which frame.
    AuiFrame(wxWindow *inParent,
              wxWindowID inId,
              const wxString &inTitle,
              const wxString &inFrameName,
              const wxSize &inSize = wxDefaultSize,
              long inStyle = wxDEFAULT_FRAME_STYLE,
              const wxString &inName = wxFrameNameStr);

    /// Lay out the child elements of this window.  Overridden so that we can
    /// update our window size after mAuiManager has laid out the window.
    virtual bool Layout();

private:
    /// Returns true iff inRect is entirely on one of the system's
    /// displays.  Used for trying to put windows in the same place they
    /// were the last time they were closed, but only if that's a
    /// reasonably sane thing to do.
    static bool IsRectOnDisplay(const wxRect &inRect);

    /// We need to load this information *before* we load anything
    /// else, because there's no portable way to change it once the window
    /// is created.
    static wxPoint LoadFramePosition(const wxString &inFrameName);

    /// Recalculate the current frame's size.
    void UpdateMinimumFrameSize();

    /// Load mDefaultPerspective.
    void OnResetPerspective(wxCommandEvent &inEvent);

protected:
    /// Get a configuration object with its path set to the appropriate
    /// value for this frame.
    shared_ptr<wxConfigBase> GetConfigForFrame();

    /// Load the perspective for the current frame.
    void LoadFramePerspective();

    /// Save the perspective for the current frame if it's safe to do so.
    void MaybeSaveFramePerspective();

    /// Call mAuiManager->Update() to lay out the frame, record
    /// mDefaultPerspective, and load any saved perspective.  Must be
    /// called by all subclasses after all panes have been configured in
    /// the constructor.
    void FinishSettingUpAuiManager();

    /// Save our current perspective, and uninitialize mAuiManager.  Must
    /// be called by all subclasses before calling Destroy, typically in an
    /// OnClose event handler after the decision has been made to close the
    /// frame.
    void ShutDownAuiManager();
};

#endif // AuiFrame_H
