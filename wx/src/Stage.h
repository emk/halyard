// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Stage_H
#define Stage_H

class Stage;

//////////
// Our main window--the "frame" around our stage.
//
class StageFrame : public wxFrame
{
    //////////
    // A separate top-level window which logs a variety of interesting
    // events.
    //
    wxLogWindow *mLogWindow;

    //////////
    // Our most important child--the actual "stage" itself on which our
    // multimedia programs run.
    //
    Stage *mStage;

    // Menus, etc.
    wxMenuBar *mMenuBar;
    wxMenu *mFileMenu;
    wxMenu *mViewMenu;
    wxMenu *mHelpMenu;

public:
    //////////
    // Create and display a new stage frame.
    //
    // [in] inTitle - The window title.
    // [in] inStageSize - The size of our stage.
    //
    StageFrame(const wxChar *inTitle, wxSize inStageSize);

    void OnExit();
    void OnReloadScript();
    void OnAbout();

    void OnShowLog();
    void UpdateUiShowFullScreen(wxUpdateUIEvent &inEvent);
    void OnShowFullScreen();
    void UpdateUiShowXy(wxUpdateUIEvent &inEvent);
    void OnShowXy();

    //////////
    // We provide an OnClose event handler so we can notify the application
    // object when our window is closed.  This function could also be used
    // to implement an "are you sure you want to quit?" window.
    //
    void OnClose(wxCloseEvent &inEvent);

    DECLARE_EVENT_TABLE();
};

class Stage : public wxWindow
{
    //////////
    // The StageFrame associated with the stage.  We need to poke at it
    // occassionally to implement various features.
    //
    StageFrame *mFrame;

    //////////
    // Are we displaying the XY co-ordinates of the cursor?
    //
    bool mIsShowingXy;

public:
    //////////
    // Create a new stage.  Should only be called by StageFrame.
    //
    // [in] inParent - The immediate parent of this stage.
    // [in] inFrame - The StageFrame in which this stage appears.
    //                Probably not the same as inParent.
    // [in] inStageSize - The size of the stage.
    //
    Stage(wxWindow *inParent, StageFrame *inFrame, wxSize inStageSize);

    //////////
    // Trap mouse movement events so we can do various useful things.
    //
    void OnMouseMove(wxMouseEvent &inEvent);

    //////////
    // Are we currently displaying the XY co-ordinates of the cursor?
    //
    bool IsShowingXy() { return mIsShowingXy; }

    //////////
    // Toggle the display of the cursor's XY co-ordinates.
    //
    void ToggleShowXy() { mIsShowingXy = !mIsShowingXy; }

    DECLARE_EVENT_TABLE();
};

#endif // Stage_H
