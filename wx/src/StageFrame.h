// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#include <wx/display.h>
#include "ModelView.h"
#include "SashFrame.h"

BEGIN_NAMESPACE_FIVEL
class Document;
END_NAMESPACE_FIVEL

class Stage;
class LocationBox;
class ProgramTree;
class ToolWindow;
class StageBackground;
class wxSashEvent;

// See ToolWindow.h.
enum ToolWindowID {
	TOOL_LISTENER,
	TOOL_TIMECODER,

	// This needs to be last in the list.  It's the total number of tool
	// windows tracked by the StageFrame.
	TOOL_COUNT
};


//////////
/// Our main window--the "frame" around our stage.
///
class StageFrame : public SashFrame, public model::View
{
	//////////
	/// Our associated document object.
	///
	FIVEL_NS Document *mDocument;

    //////////
    /// A separate top-level window which logs a variety of interesting
    /// events.
    ///
    wxLogWindow *mLogWindow;

    //////////
    /// An interactive listener window.
    ///
    ToolWindow *mToolWindows[TOOL_COUNT];

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

    // Menus, etc.
    wxMenuBar *mMenuBar;
    wxMenu *mFileMenu;
    wxMenu *mCardMenu;
    wxMenu *mViewMenu;
    wxMenu *mInsertMenu;
    wxMenu *mWindowMenu;
    wxMenu *mHelpMenu;

    //////////
    /// Does UpdateVideoMode believe that our full-screen options are
    /// currently active?
    ///
    /// \see UpdateVideoMode
    ///
    bool mAreFullScreenOptionsActive;
    
	//////////
	/// The minimum allowable size of our stage frame.  We save this so
	/// we can temporarily change it for full-screen mode.
	///
	wxSize mMinimumFrameSize;

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

    void LoadSashLayout(wxConfigBase *inConfig);
    void SaveSashLayout(wxConfigBase *inConfig);

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
    FIVEL_NS Document *GetDocument() { return mDocument; }

    //////////
    /// Get the location box attached to this frame.
    ///
    LocationBox *GetLocationBox() { return mLocationBox; }

    //////////
    /// Get the program tree attached to this frame.
    ///
    ProgramTree *GetProgramTree() { return mProgramTree; }

	//////////
	/// Notify the StageFrame that the specified tool window is being
	/// destroyed.  This should only be called by the tool window
	/// itself.
	///
	void DetachToolWindow(ToolWindowID inTool) { mToolWindows[inTool] = NULL; }

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
	/// We listen to the Document object so we can update the frame
	/// title.
	///
	void ObjectChanged();

	//////////
	/// We listen to the Document object so we can update the frame
	/// title.
	///
	void ObjectDeleted();

#ifdef FIVEL_PLATFORM_WIN32

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


#endif // FIVEL_PLATFORM_WIN32

private:
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
    void OnShowTimecoder(wxCommandEvent &inEvent);
    void UpdateUiFullScreen(wxUpdateUIEvent &inEvent);
    void OnFullScreen(wxCommandEvent &inEvent);
    void UpdateUiDisplayXy(wxUpdateUIEvent &inEvent);
    void OnDisplayXy(wxCommandEvent &inEvent);
    void UpdateUiDisplayGrid(wxUpdateUIEvent &inEvent);
    void OnDisplayGrid(wxCommandEvent &inEvent);
    void UpdateUiDisplayBorders(wxUpdateUIEvent &inEvent);
    void OnDisplayBorders(wxCommandEvent &inEvent);
    void UpdateUiProperties(wxUpdateUIEvent &inEvent);
    void OnProperties(wxCommandEvent &inEvent);
    void UpdateUiInsertBackground(wxUpdateUIEvent &inEvent);
    void OnInsertBackground(wxCommandEvent &inEvent);
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

    //////////
    /// We provide an OnClose event handler so we can notify the application
    /// object when our window is closed.  This function could also be used
    /// to implement an "are you sure you want to quit?" window.
    ///
    void OnClose(wxCloseEvent &inEvent);

    DECLARE_EVENT_TABLE();
};

#endif // StageFrame_H
