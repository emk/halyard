// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Stage_H
#define Stage_H

#include <deque>

#include "GraphicsTools.h"
#include "AppGlobals.h"

class Stage;
class Element;
class LocationBox;
class ToolWindow;
class TQTMovie;
class MovieElement;

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
class StageFrame : public wxFrame
{
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

    // Menus, etc.
    wxMenuBar *mMenuBar;
    wxMenu *mFileMenu;
    wxMenu *mCardMenu;
    wxMenu *mViewMenu;
    wxMenu *mWindowMenu;
    wxMenu *mHelpMenu;

	//////////
	// The drop-down box which allows us to jump between cards.
	//
	LocationBox *mLocationBox;

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

    //////////
    // Get the location box attached to this frame.
    //
    LocationBox *GetLocationBox() { return mLocationBox; }

	//////////
	// Notify the StageFrame that the specified tool window is being
	// destroyed.  This should only be called by the tool window
	// itself.
	//
	void DetachToolWindow(ToolWindowID inTool) { mToolWindows[inTool] = NULL; }

    void OnExit(wxCommandEvent &inEvent);
    void OnReloadScript(wxCommandEvent &inEvent);
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
    void UpdateUiJumpCard(wxUpdateUIEvent &inEvent);
    void OnJumpCard(wxCommandEvent &inEvent);

    //////////
    // We provide an OnClose event handler so we can notify the application
    // object when our window is closed.  This function could also be used
    // to implement an "are you sure you want to quit?" window.
    //
    void OnClose(wxCloseEvent &inEvent);

    DECLARE_EVENT_TABLE();
};

class Stage : public wxWindow, public GraphicsTools::Image
{
	//////////
	// A list of Elements.
	//
	typedef std::deque<Element*> ElementCollection;

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
	// Our text-entry control.  Will not be visible unless the user
	// is entering something.
	//
	wxTextCtrl *mTextCtrl;

	//////////
	// Our currently active elements.
	//
	ElementCollection mElements;

	//////////
	// The element which most recently contained the mouse.
	// Invariant: This variable is always NULL, or points to a valid 
	// lightweight element.  Be careful when deleting element!
	//
	Element* mCurrentElement;

	//////////
	// The movie we're waiting on, or NULL if we're not waiting on anything.
	//
	MovieElement *mWaitElement;

	//////////
	// The movie frame we're waiting on.
	//
	MovieFrame mWaitFrame;

    //////////
    // Are we displaying the XY co-ordinates of the cursor?
    //
    bool mIsDisplayingXy;

    //////////
    // Are we displaying the grid over the stage?
    //
    bool mIsDisplayingGrid;

    //////////
    // Are we displaying borders for the interactive elements.
    //
    bool mIsDisplayingBorders;

	//////////
	// Invalidate the entire stage.
	//
	void InvalidateStage();

	//////////
	// Invalidate the specified rectangle.
	//
	void InvalidateRect(const wxRect &inRect);

    //////////
    // Draw a border for the specified rectangle.
    //
    void DrawElementBorder(wxDC &inDC, const wxRect &inElementRect);

	//////////
	// End an active Wait().
	//
	void EndWait();

	//////////
	// Put the interpreter to sleep.
	//
	void InterpreterSleep();

	//////////
	// Wake the interpreter up.
	//
	void InterpreterWakeUp();

	//////////
	// Find an element by name, and return an iterator.
	//
	ElementCollection::iterator
	FindElementByName(ElementCollection &inCollection,
					  const wxString &inName);

	//////////
	// Detach an element from the stage and destroy it.
	//
	void DestroyElement(Element *inElement);

    //////////
    // Let the stage know that the list of active elements has changed.
    //
    void NotifyElementsChanged();
	
	//////////
	// Figure out which element we're inside, and figure out what cursor
	// we should be displaying now.
	//
	void UpdateCurrentElementAndCursor();

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
	// We need to clean up any resources which aren't directly managed
	// by wxWindows.
	//
    ~Stage();

    //////////
    // Notify the stage that the interpreter has moved to a new card.
    //
    void NotifyEnterCard();

    //////////
    // Notify the stage that the interpreter is leaving an old card.
    //
    void NotifyExitCard();

    //////////
    // Notify the stage that the script is being reloaded.
    //
    void NotifyScriptReload();

    //////////
    // Do our idle-time processing.
    //
    void OnIdle(wxIdleEvent &inEvent);

    //////////
    // Trap mouse movement events so we can do various useful things.
    //
    void OnMouseMove(wxMouseEvent &inEvent);

    //////////
    // Intercept the erase event to prevent flicker.
    //
    void OnEraseBackground(wxEraseEvent &inEvent);

    //////////
    // Redraw the stage.
    //
    void OnPaint(wxPaintEvent &inEvent);

    //////////
    // Intercept ENTER in a text field.
    //
    void OnTextEnter(wxCommandEvent &inEvent);

    //////////
    // Handle a mouse-down event.
    //
    void OnLeftDown(wxMouseEvent &inEvent);

    //////////
    // Are we currently displaying the XY co-ordinates of the cursor?
    //
    bool IsDisplayingXy() { return mIsDisplayingXy; }

    //////////
    // Toggle the display of the cursor's XY co-ordinates.
    //
    void ToggleDisplayXy() { mIsDisplayingXy = !mIsDisplayingXy; }

    //////////
    // Are we currently displaying the grid?
    //
    bool IsDisplayingGrid() { return mIsDisplayingGrid; }

    //////////
    // Toggle the display of the grid.
    //
    void ToggleDisplayGrid()
		{ InvalidateStage(); mIsDisplayingGrid = !mIsDisplayingGrid; }

    //////////
    // Are we currently displaying the borders?
    //
    bool IsDisplayingBorders() { return mIsDisplayingBorders; }

    //////////
    // Toggle the display of the borders.
    //
    void ToggleDisplayBorders()
		{ InvalidateStage(); mIsDisplayingBorders = !mIsDisplayingBorders; }

	//////////
	// Handy conversion operator to transform 5L colors into wxWindows colors.
	//
	wxColor GetColor(const GraphicsTools::Color &inColor);

    //////////
    // Clear the stage to the specified color.
    //
    void ClearStage(const wxColor &inColor);

	//////////
	// Fill in the specified box with the specified color.
	//
	void FillBox(const wxRect &inBounds, const wxColour &inColor);

	//////////
	// Draw a portable PixMap to the screen, blending alpha
	// values appropriately.
	//
	// [in] inPoint - The location at which to draw the pixmap.
	// [in] inPixMap - The pixmap to draw.
	//
	void		DrawPixMap(GraphicsTools::Point inPoint,
						   GraphicsTools::PixMap &inPixMap);

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

	//////////
	// Display a modal text input box.
	//
	// [in] inBounds - The box to use for text input.
	// [in] inTextSize - The input text size, in points.
	// [in] inForeColor - The color of the text.
	// [in] inBackColor - The color of the background.
	//
	void ModalTextInput(const wxRect &inBounds,
						const int inTextSize,
						const wxColour &inForeColor,
						const wxColour &inBackColor);

	//////////
	// Finish using the box created by ModalTextInput.
	//
	// [out] return - The text entered by the user.
	//
	wxString FinishModalTextInput();

	//////////
	// Suspend the interpreter until the named movie reaches the specified
	// frame.
	//
	// [in] inElementName - The name of the MovieElement to wait on.
	// [in] inUntilFrame - The frame to wait until.
	// [out] return - true if the wait request was valid, false if the
	//                named element doesn't exist or isn't a movie.
	//
	bool Wait(const wxString &inElementName, MovieFrame inUntilFrame);

	//////////
	// Add a Element to this Stage.  This should only be called
	// by the Element class.
	//
	void AddElement(Element *inElement);

	//////////
	// Find the lightweight Element containing the specified point, if
	// any.  We normally call this function when we want to find a
	// lightweight element to handle some kind of mouse event.
	//
	// [in] inPoint - The point to check.
	// [out] return - A pointer to the Element, or NULL.
	//
	Element *FindLightWeightElement(const wxPoint &inPoint);

	//////////
	// Delete a Element by name.
	//
	// [in] inName - The name of the Element to delete.
	// [out] return - Returns true if that Element existed.
	//
	bool DeleteElementByName(const wxString &inName);

	//////////
	// Delete all Elements owned the Stage.
	//
	void DeleteElements();

    DECLARE_EVENT_TABLE();
};

#endif // Stage_H
