// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Stage_H
#define Stage_H

#include <deque>

#include "GraphicsTools.h"

class Stage;
class StageObject;
class LocationBox;
class Listener;


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
    Listener *mListenerWindow;

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
	// Notify the StageFrame that the listener window is being
	// destroyed.  This should only be called by the Listener.
	//
	void DetachListenerWindow() { mListenerWindow = NULL; }

    void OnExit();
    void OnReloadScript();
    void OnAbout();

    void OnShowLog();
    void OnShowListener();
    void UpdateUiFullScreen(wxUpdateUIEvent &inEvent);
    void OnFullScreen();
    void UpdateUiDisplayXy(wxUpdateUIEvent &inEvent);
    void OnDisplayXy();
    void UpdateUiDisplayGrid(wxUpdateUIEvent &inEvent);
    void OnDisplayGrid();
    void UpdateUiDisplayBorders(wxUpdateUIEvent &inEvent);
    void OnDisplayBorders();
    void UpdateUiJumpCard(wxUpdateUIEvent &inEvent);
    void OnJumpCard();

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
	// A list of StageObjects.
	//
	typedef std::deque<StageObject*> StageObjectCollection;

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
	// Our currently active stage objects.
	//
	StageObjectCollection mStageObjects;

	//////////
	// The stage object which most recently contained the mouse.
	// Invariant: This variable is always NULL, or points to a valid 
	// lightweight stage object.  Be careful when deleting stage objects!
	//
	StageObject* mLastStageObject;

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
    void DrawObjectBorder(wxDC &inDC, const wxRect &inObjectRect);

    //////////
    // Let the stage know that the list of active objects has changed.
    //
    void NotifyObjectsChanged();

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
	// Add a StageObject to this Stage.  This should only be called
	// by the StageObject class.
	//
	void AddStageObject(StageObject *inStageObject);

	//////////
	// Find the lightweight StageObject containing the specified point, if
	// any.  We normally call this function when we want to find a
	// lightweight stage object to handle some kind of mouse event.
	//
	// [in] inPoint - The point to check.
	// [out] return - A pointer to the StageObject, or NULL.
	//
	StageObject *FindLightWeightStageObject(const wxPoint &inPoint);

	//////////
	// Delete a StageObject by name.
	//
	// [in] inName - The name of the StageObject to delete.
	// [out] return - Returns true if that StageObject existed.
	//
	bool DeleteStageObjectByName(const wxString &inName);

	//////////
	// Delete all StageObjects owned the Stage.
	//
	void DeleteStageObjects();

    DECLARE_EVENT_TABLE();
};

//////////
// A StageObject is something which appears on a stage--typically either
// a Zone or an full-fledged widget.  StageObjects are either "lightweight",
// meaning they need help from the stage to process events, or they are
// heavyweight and use wxWindows to process their events.  Zones are
// lightweight; widgets are heavyweight.
//
// This class has a "fat" interface--a number of methods are only useful
// if IsLightWeight returns true.  This allows us to avoid using RTTI,
// but is otherwise a slightly odd design.
//
class StageObject
{
	//////////
	// The stage on which this object appears.
	//
	Stage *mStage;

	//////////
	// The name of this object.  Must be unique on any given card. 
	//
	wxString mName;

public:
	//////////
	// Create a new StageObject and attach it to the specified stage.
	// The stage is responsible for deleting the object.
	//
	StageObject(Stage *inStage, const wxString &inName);

	virtual ~StageObject() {}
	
	//////////
	// Return the name of the stage object.  Should be unique on any
	// given card.
	//
	wxString GetName() { return mName; }

	//////////
	// Return the bounding box of the stage object.  Used for drawing
	// borders around the object.
	//
	virtual wxRect GetRect() = 0;

	//////////
	// Does this object need to receive
	//
	virtual bool IsLightWeight() { return false; }

	//////////
	// Is the specified point in the object?
	// NOT USEFUL UNLESS IsLightWeight RETURNS TRUE.
	//
	virtual bool IsPointInStageObject(const wxPoint &inPoint) { return false; }

	//////////
	// Pass a click event to the object.
	// NOT USEFUL UNLESS IsLightWeight RETURNS TRUE.
	//
	virtual void Click() { }
};

#endif // Stage_H
