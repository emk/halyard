// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Stage_H
#define Stage_H

#include "GraphicsTools.h"

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
    wxMenu *mWindowMenu;
    wxMenu *mHelpMenu;

public:
    //////////
    // Create and display a new stage frame.
    //
    // [in] inTitle - The window title.
    // [in] inStageSize - The size of our stage.
    //
    StageFrame(const wxChar *inTitle, wxSize inStageSize);

    //////////
    // Get the stage attached to this frame.
    //
    Stage *GetStage() { return mStage; }

    void OnExit();
    void OnReloadScript();
    void OnAbout();

    void OnShowLog();
    void UpdateUiFullScreen(wxUpdateUIEvent &inEvent);
    void OnFullScreen();
    void UpdateUiDisplayXy(wxUpdateUIEvent &inEvent);
    void OnDisplayXy();

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
    // The size of our drawing stage.
    //
    wxSize mStageSize;

    //////////
    // The StageFrame associated with the stage.  We need to poke at it
    // occassionally to implement various features.
    //
    wxBitmap mOffscreenPixmap;

    //////////
    // Are we displaying the XY co-ordinates of the cursor?
    //
    bool mIsDisplayingXy;

	//////////
	// Invalidate the specified rectangle.
	//
	void InvalidateRect(const wxRect &inRect);

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
    // Redraw the stage.
    //
    void OnPaint(wxPaintEvent &inEvent);

    //////////
    // Are we currently displaying the XY co-ordinates of the cursor?
    //
    bool IsDisplayingXy() { return mIsDisplayingXy; }

    //////////
    // Toggle the display of the cursor's XY co-ordinates.
    //
    void ToggleDisplayXy() { mIsDisplayingXy = !mIsDisplayingXy; }

	//////////
	// Handy conversion operator to transform 5L colors into wxWindows colors.
	//
	wxColor GetColor(const GraphicsTools::Color &inColor);

    //////////
    // Clear the stage to the specified color.
    //
    void ClearStage(const wxColor &inColor);

    //////////
    // Draw a bitmap on the stage at the specified location.
	//
	// [in] inBitmap - The bitmap to draw.
	// [in] inX - The X coordinate to draw it at.
	// [in] inY - The Y coordinate to draw it at.
	// [in_optional] inTransparent - Should we honor transparency information
	//                               in the bitmap?
    //
    void DrawBitmap(const wxBitmap &inBitmap, wxCoord inX, wxCoord inY,
					bool inTransparent = true);

    DECLARE_EVENT_TABLE();
};

#endif // Stage_H
