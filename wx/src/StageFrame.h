// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef StageFrame_H
#define StageFrame_H

#include <wx/display.h>
#include "ModelView.h"

class FIVEL_NS Document;
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
// Our main window--the "frame" around our stage.
//
class StageFrame : public wxFrame, public model::View
{
	//////////
	// Our associated document object.
	//
	FIVEL_NS Document *mDocument;

    //////////
    // A separate top-level window which logs a variety of interesting
    // events.
    //
    wxLogWindow *mLogWindow;

    //////////
    // An interactive listener window.
    //
    ToolWindow *mToolWindows[TOOL_COUNT];

    //////////
    // Our most important child--the actual "stage" itself on which our
    // multimedia programs run.
    //
    Stage *mStage;

	//////////
	// The frame surrounding our stage.  This is just empty space.
	//
	StageBackground *mBackground;

	//////////
	// A window which displays a tree of all the interesting information
	// in our program.
	//
	ProgramTree *mProgramTree;

	//////////
	// The drop-down box which allows us to jump between cards.
	//
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
	// The minimum allowable size of our stage frame.  We save this so
	// we can temporarily change it for full-screen mode.
	//
	wxSize mMinimumFrameSize;

	//////////
	// Have we re-loaded our layout?
	//
	bool mHaveLoadedFrameLayout;

	//////////
	// We need to load this layout information *before* we load anything
	// else, because there's no portable way to change it once the window
	// is created.
	//
	static wxPoint LoadFramePosition();

	//////////
	// Load the layout for the current frame.
	//
	void LoadFrameLayout();

	//////////
	// Save the layout for the current frame if it's safe to do so.
	//
	void MaybeSaveFrameLayout();

	//////////
	// The best video mode for full-screen displays.  We calculate this
	// once at startup.
	//
#if wxUSE_DISPLAY
	wxVideoMode mFullScreenVideoMode;
#endif

	//////////
	// Calculate the best video mode to use for full-screen displays.
	//
	void FindBestFullScreenVideoMode();

	//////////
	// Switch the video mode to full-screen, if possible.
	//
	void SetFullScreenVideoMode();

	//////////
	// Switch the video mode back to normal, if possible.
	//
	void ResetVideoMode();

public:
    //////////
    // Create and display a new stage frame.
    //
    // [in] inStageSize - The size of our stage.
    //
    StageFrame(wxSize inStageSize);

    //////////
    // Get the stage attached to this frame.
    //
    Stage *GetStage() { return mStage; }

    //////////
    // Get the location box attached to this frame.
    //
    LocationBox *GetLocationBox() { return mLocationBox; }

    //////////
    // Get the program tree attached to this frame.
    //
    ProgramTree *GetProgramTree() { return mProgramTree; }

	//////////
	// Notify the StageFrame that the specified tool window is being
	// destroyed.  This should only be called by the tool window
	// itself.
	//
	void DetachToolWindow(ToolWindowID inTool) { mToolWindows[inTool] = NULL; }

	//////////
	// Override wxFrame's ShowFullScreen method so we can hide some
	// distracting visual clutter.
	//
    virtual bool ShowFullScreen(bool show, long style = wxFULLSCREEN_ALL);

	//////////
	// Create a new document in the current frame.
	//
	void NewDocument();

	//////////
	// Create a new document in the current frame.
	//
	void OpenDocument();

	//////////
	// We listen to the Document object so we can update the frame
	// title.
	//
	void ObjectChanged();

	//////////
	// We listen to the Document object so we can update the frame
	// title.
	//
	void ObjectDeleted();

private:
	// Lots of menu and toolbar event handlers.
    void OnExit(wxCommandEvent &inEvent);
    void UpdateUiNewProgram(wxUpdateUIEvent &inEvent);
    void OnNewProgram(wxCommandEvent &inEvent);
    void UpdateUiOpenProgram(wxUpdateUIEvent &inEvent);
    void OnOpenProgram(wxCommandEvent &inEvent);
    void UpdateUiSaveProgram(wxUpdateUIEvent &inEvent);
    void OnSaveProgram(wxCommandEvent &inEvent);
    void OnReloadScript(wxCommandEvent &inEvent);
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
    void UpdateUiJumpCard(wxUpdateUIEvent &inEvent);
    void OnJumpCard(wxCommandEvent &inEvent);
    void UpdateUiStopMovies(wxUpdateUIEvent &inEvent);
    void OnStopMovies(wxCommandEvent &inEvent);

	//////////
	// "Sashes" are narrow bars between subwindows in frame.  When
	// a sash in the main window is dragged, it generates an event
	// which we process here.
	//
	void OnSashDrag(wxSashEvent &inEvent);

    //////////
	// We provide an OnSize handler so we can handle window resizing
	// gracefully.
	//
	void OnSize(wxSizeEvent &inEvent);

    //////////
    // We provide an OnClose event handler so we can notify the application
    // object when our window is closed.  This function could also be used
    // to implement an "are you sure you want to quit?" window.
    //
    void OnClose(wxCloseEvent &inEvent);

    DECLARE_EVENT_TABLE();
};

#endif // StageFrame_H
