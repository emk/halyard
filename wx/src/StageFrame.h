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

#ifndef StageFrame_H
#define StageFrame_H

#include "TInterpreter.h"
#include <wx/display.h>
#include "ModelView.h"
#include "AuiFrame.h"

BEGIN_NAMESPACE_HALYARD
class Document;
END_NAMESPACE_HALYARD

class Stage;
class LocationBox;
class ProgramTree;
class StageBackground;
class Listener;
class MediaInfoPane;


//////////
/// Our main window--the "frame" around our stage.
///
class StageFrame : public AuiFrame,
                   public Halyard::TReloadNotified,
                   public model::View
{
	//////////
	/// Our associated document object.
	///
	Halyard::Document *mDocument;

    //////////
    /// A separate top-level window which logs a variety of interesting
    /// events.
    ///
    wxLogWindow *mLogWindow;

    //////////
    /// Our most important child--the actual "stage" itself on which our
    /// multimedia programs run.
    ///
    Stage *mStage;

	//////////
	/// The frame surrounding our stage.  This is just empty space.
	///
	StageBackground *mBackground;

	//////////
	/// A window which displays a tree of all the interesting information
	/// in our program.
	///
	ProgramTree *mProgramTree;

	//////////
	/// The drop-down box which allows us to jump between cards.
	///
	LocationBox *mLocationBox;

    //////////
    /// Our "listener", which allows users to execute small code snippets.
    ///
    Listener *mListener;

    //////////
    /// A dockable pane showing information about the currently playing
    /// media clip.
    ///
    MediaInfoPane *mMediaInfoPane;

    // Menus, etc.
    wxMenuBar *mMenuBar;
    wxMenu *mFileMenu;
    wxMenu *mCardMenu;
    wxMenu *mViewMenu;
    wxMenu *mInsertMenu;
    wxMenu *mWindowMenu;
    wxMenu *mHelpMenu;

    //////////
    /// We use this clock to time how long a reload takes.
    ///
    wxStopWatch mReloadStopWatch;

    //////////
    /// Are developer tools available in all modes, and not just authoring
    /// mode?
    ///
    bool mAreDevToolsAvailableInAllModes;

    //////////
    /// Does UpdateVideoMode believe that our full-screen options are
    /// currently active?
    ///
    /// \see UpdateVideoMode
    ///
    bool mAreFullScreenOptionsActive;
    
    //////////
    /// The wxDisplay ID number of the screen we intend to use for
    /// full-screen mode.  We update this shortly before switching
    /// into full-screen mode.
    ///
    int mCurrentFullScreenDisplayId;

#if wxUSE_DISPLAY

	//////////
	/// The best video mode for full-screen displays.  We calculate this
	/// once at startup.
	///
	wxVideoMode mFullScreenVideoMode;

    //////////
    /// The name of the wxConfig key we'll use to store our screen-resizing
    /// preferences.
    ///
    wxString mResizePrefName;

#endif // wxUSE_DISPLAY

	//////////
	/// Used to prevent endless recursion if we pop up a dialog in OnActivate.
	///
	bool mIsUpdatingVideoMode;

    //////////
    /// When we switch to full screen mode, we save our wxAui perspective.
    ///
    wxString mLastPerspectiveBeforeFullScreenMode;

	//////////
	/// Calculate the best video mode to use for full-screen displays.
	///
	void FindBestFullScreenVideoMode();

	//////////
	/// Switch the video mode to full-screen, if possible.
	///
	void SetFullScreenVideoMode();

	//////////
	/// Switch the video mode back to normal, if possible.
	///
	void ResetVideoMode();

#if wxUSE_DISPLAY

    //////////
    /// Figure out what wxConfig key we should use to store our resizing
    /// preferences.  This is dependent on the display geometry when our
    /// application is first launched, because we want to store separate
    /// preferences for separate display configurations.  This affects
    /// laptops with external screens, rotatable monitors, and other such
    /// beasts.
    ///
    void ComputeResizePrefName();

    //////////
    /// Should we resize the screen?  (May interact with the user.)
    ///
    bool ShouldResizeScreen(bool &outShouldConfirm);
    
    //////////
    /// Is the current screen size OK?  Interacts with the user.
    ///
    bool ConfirmScreenSize();

#endif // wxUSE_DISPLAY

    //////////
    /// Update our video mode (and related parameters) for our current
    /// window status.
    ///
    void UpdateVideoMode(bool inIsFullScreen, bool inIsIconized);

    //////////
    /// Copy files from inTemplateName into inDir, overwriting an exisiting
    /// files.
    ///
    void UseTemplate(const wxString &inDir, const wxString &inTemplateName);

    //////////
    /// Call this function to everything up after a value has been assigned
    /// to mDocument.
    ///
    void FinishOpeningDocument(const wxString &inDirPath);

    //////////
    /// Try to load an icon with the specified name into our bundle. If
    /// we succeed, set ioHaveIcon to true.
    ///
    void LoadIcon(const std::string &inName, wxIconBundle &ioIcons,
                  bool &ioHaveIcon);

    //////////
    /// Return true if the screensaver should be disabled.
    ///
    bool ShouldDisableScreenSaver();

public:
    //////////
    /// Create and display a new stage frame.
    ///
    /// \param inStageSize  The size of our stage.
    ///
    StageFrame(wxSize inStageSize);

    //////////
    /// Get the stage attached to this frame.
    ///
    Stage *GetStage() { return mStage; }

    //////////
    /// Get the document associated with this stage frame.
    ///
    Halyard::Document *GetDocument() { return mDocument; }

    //////////
    /// Get the location box attached to this frame.
    ///
    LocationBox *GetLocationBox() { return mLocationBox; }

    //////////
    /// Get the program tree attached to this frame.
    ///
    ProgramTree *GetProgramTree() { return mProgramTree; }

    //////////
    /// Get the MediaInfoPane attached to this frame.
    ///
    MediaInfoPane *GetMediaInfoPane() { return mMediaInfoPane; }

	//////////
	/// Override wxFrame's ShowFullScreen method so we can hide some
	/// distracting visual clutter.
	///
    virtual bool ShowFullScreen(bool show, long style = wxFULLSCREEN_ALL);

    //////////
    /// We need to restore the screen resolution when we get Iconized.
    ///
    virtual void Iconize(bool iconize = TRUE);

    //////////
    /// If we're in authoring mode, add inDirPath to our wxFileHistory.
    ///
    static void MaybeAddToRecentProgramList(const wxString &inDirPath);

	//////////
	/// Create a new document in the current frame.
	///
	void NewDocument();

	//////////
	/// Create a new document in the current frame.
	///
	void OpenDocument();

    //////////
    /// Given a directory name, open up the document it contains.
    ///
	void OpenDocument(const wxString &inDirPath);

    //////////
    /// Make sure that no update is currently underway.
    ///
    void CheckForUpdateLockFile();

    //////////
    /// Called to notify us that the script is being reloaded.
    ///
    void NotifyReloadScriptStarting();

    //////////
    /// Called to notify us that the script has been reloaded successfully.
    ///
    void NotifyReloadScriptSucceeded();

    //////////
    /// Called to notify us that the script has failed to reload.
    ///
    void NotifyReloadScriptFailed();

	//////////
	/// We listen to the Document object so we can update the frame
	/// title.
	///
	void ObjectChanged();

	//////////
	/// We listen to the Document object so we can update the frame
	/// title.
	///
	void ObjectDeleted();

    //////////
    /// Turn on developer tools even if we're not in authoring mode.
    ///
    void EnableDeveloperToolsInAllModes()
        { mAreDevToolsAvailableInAllModes = true; }

#ifdef __WXMSW__

    //////////
    /// Sadly, we need to override this function because Windows doesn't
    /// seem to correctly update our accelerator keys if we've launched
    /// straight into full-screen mode without ever showing a menu bar.
    ///
    bool MSWTranslateMessage(WXMSG* pMsg);

    //////////
    /// We override this function so we can detect attempts to activate
    /// the screensaver.
    ///
    WXLRESULT MSWWindowProc(WXUINT message, WXWPARAM wParam, WXLPARAM lParam);


#endif // __WXMSW__

private:
    bool SaveAllForReloadScript();
    bool TryToCloseScriptEditor(wxCloseEvent &inEvent);
    bool AreDevToolsAvailable();

	// Lots of menu and toolbar event handlers.
    void UpdateUiDevTool(wxUpdateUIEvent &inEvent);
    void OnExit(wxCommandEvent &inEvent);
    void UpdateUiNewProgram(wxUpdateUIEvent &inEvent);
    void OnNewProgram(wxCommandEvent &inEvent);
    void UpdateUiOpenProgram(wxUpdateUIEvent &inEvent);
    void OnOpenProgram(wxCommandEvent &inEvent);
    void UpdateUiSaveProgram(wxUpdateUIEvent &inEvent);
    void OnSaveProgram(wxCommandEvent &inEvent);
    void OnEditScripts(wxCommandEvent &inEvent);
    void OnUpdateUiReloadScripts(wxUpdateUIEvent &inEvent);
    void OnReloadScripts(wxCommandEvent &inEvent);
    void OnRunTests(wxCommandEvent &inEvent);
    void OnAbout(wxCommandEvent &inEvent);
    void OnShowLog(wxCommandEvent &inEvent);
    void OnShowListener(wxCommandEvent &inEvent);
    void OnShowMediaInfo(wxCommandEvent &inEvent);
    void UpdateUiFullScreen(wxUpdateUIEvent &inEvent);
    void OnFullScreen(wxCommandEvent &inEvent);
    void UpdateUiDisplayXy(wxUpdateUIEvent &inEvent);
    void OnDisplayXy(wxCommandEvent &inEvent);
    void UpdateUiDisplayGrid(wxUpdateUIEvent &inEvent);
    void OnDisplayGrid(wxCommandEvent &inEvent);
    void UpdateUiDisplayBorders(wxUpdateUIEvent &inEvent);
    void OnDisplayBorders(wxCommandEvent &inEvent);
    void UpdateUiErrortraceCompile(wxUpdateUIEvent &inEvent);
    void OnErrortraceCompile(wxCommandEvent &inEvent);
    void UpdateUiProperties(wxUpdateUIEvent &inEvent);
    void OnProperties(wxCommandEvent &inEvent);
    void UpdateUiEditMode(wxUpdateUIEvent &inEvent);
    void OnEditMode(wxCommandEvent &inEvent);
    void UpdateUiEditCardScript(wxUpdateUIEvent &inEvent);
    void OnEditCardScript(wxCommandEvent &inEvent);
    void UpdateUiJumpCard(wxUpdateUIEvent &inEvent);
    void OnJumpCard(wxCommandEvent &inEvent);
    void UpdateUiStopMovies(wxUpdateUIEvent &inEvent);
    void OnStopMovies(wxCommandEvent &inEvent);

    //////////
    /// When our window is activated, we need to make sure that
    /// UpdateVideoMode has been called properly.
    ///
	void OnActivate(wxActivateEvent &inEvent);

    //////////
    /// We provide an OnClose event handler so we can notify the application
    /// object when our window is closed.  This function could also be used
    /// to implement an "are you sure you want to quit?" window.
    ///
    void OnClose(wxCloseEvent &inEvent);

    DECLARE_EVENT_TABLE();
};

#endif // StageFrame_H
