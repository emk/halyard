// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Stage_H
#define Stage_H

#include <wx/display.h>

#include <deque>

#include "GraphicsTools.h"
#include "AppGlobals.h"
#include "ModelView.h"

class FIVEL_NS Document;
class Stage;
class Element;
class LocationBox;
class ProgramTree;
class ToolWindow;
class TQTMovie;
class MovieElement;
class StageBackground;
class wxSashEvent;
class EventDispatcher;
class ImageCache;
class CursorManager;
class TransitionManager;

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
    // The last card the stage was known to be on.
    //
    std::string mLastCard;

    //////////
    // A bitmap for storing the graphics to display on the stage.
    //
    wxBitmap mOffscreenPixmap;

    //////////
    // A bitmap for use during various fade effects.
    //
    wxBitmap mOffscreenFadePixmap;

	//////////
	// A bitmap for saving and restoring graphics
	// 
	wxBitmap mSavePixmap;

	//////////
	// We try to rate-limit our idle events to prevent performance
	// problems with the Scheme garbage collector (the event dispatching
	// system allocates some memory to process events).
	//
	wxLongLong mLastIdleEvent;

	//////////
	// This object does all of our event-dispatching for us.
	//
	EventDispatcher *mEventDispatcher;

	//////////
	// Our image cache.  This allows us to speed up image loading
	// tremendously in some common cases.
	//
	ImageCache *mImageCache;

	//////////
	// Our cursor manager.  This maps strings to cursors, and allows the
	// application to register new cursors.
	//
	CursorManager *mCursorManager;

	//////////
	// Our transition manager.  This maps transition names to transitions,
	// and applies them.
	//
	TransitionManager *mTransitionManager;

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
	// The element which has a "grab" on the mouse.
	//
	Element* mGrabbedElement;

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
	// The last stage position copied with a right-click.
	//
	wxPoint mLastCopiedPos;

	//////////
	// Invalidate the entire stage.
	//
	void InvalidateStage();

	//////////
	// Invalidate the specified rectangle.
	//
	void InvalidateRect(const wxRect &inRect);

	//////////
	// Validate the entire stage--i.e., mark it as having been redrawn.
	//
	void ValidateStage();

	//////////
	// Repaint the stage.
	//
	void PaintStage(wxDC &inDC);

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
	// We've entered an element; update things appropriately.
	//
	void EnterElement(Element *inElement, wxPoint &inPosition);

	//////////
	// We've left an element; update things appropriately.
	//
	void LeaveElement(Element *inElement, wxPoint &inPosition);

	//////////
	// Figure out which element we're inside, and figure out what cursor
	// we should be displaying now.
	//
	void UpdateCurrentElementAndCursor(wxPoint &inPosition);
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
	// Get the size of our stage.
	//
	wxSize GetStageSize() const { return mStageSize; }

	//////////
	// Return true if and only if the script is fully initialized.
	// Will briefly return false after the script is reloaded.
	//
	bool IsScriptInitialized();

	//////////
	// Place the stage into edit mode or take it out again.  May
	// only be called if IsScriptInitialized() returns true.
	//
	void SetEditMode(bool inWantEditMode = true);

	//////////
	// Is the stage in edit mode?  May only be called if
	// IsScriptInitialized() returns true.
	//
	bool IsInEditMode();

	//////////
	// Should we send events to our event dispatcher?
	//
	bool ShouldSendEvents();

	//////////
	// Return true if and only if it's safe to call TryJumpTo().
	//
	bool CanJump();

	//////////
	// Jump to the specified card.  May only be called if
	// IsScriptInitialized() returns true.  If the specified
	// card does not exist, displays an error to the user.
	//
	void TryJumpTo(const wxString &inName);

	//////////
	// Return the EventDispatcher associated with this stage.
	//
	EventDispatcher *GetEventDispatcher() { return mEventDispatcher; }

	//////////
	// Return the image cache associated with this stage.
	//
	ImageCache *GetImageCache() { return mImageCache; }

	//////////
	// Return the cursor manager associated with this stage.
	//
	CursorManager *GetCursorManager() { return mCursorManager; }

    //////////
    // Register a newly-loaded card with the stage frame.
    //
    void RegisterCard(const wxString &inName);

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
    // Let the stage know that the list of active elements has changed.
    //
    void NotifyElementsChanged();
	
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
	// Handle a character event.
	//
    void OnChar(wxKeyEvent &inEvent);

    //////////
    // Intercept ENTER in a text field.
    //
    void OnTextEnter(wxCommandEvent &inEvent);

    //////////
    // Handle a mouse-down event.
    //
    void OnLeftDown(wxMouseEvent &inEvent);

    //////////
    // Handle a mouse double-click event.
    //
    void OnLeftDClick(wxMouseEvent &inEvent);

    //////////
    // Handle a mouse-up event.
    //
    void OnLeftUp(wxMouseEvent &inEvent);

    //////////
    // Handle a mouse-down event.
    //
    void OnRightDown(wxMouseEvent &inEvent);

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
	// Draw a line in the specified color.
	//
	void DrawLine(const wxPoint &inFrom, const wxPoint &inTo,
				  const wxColour &inColor, int inWidth);

	//////////
	// Fill in the specified box with the specified color.
	//
	void FillBox(const wxRect &inBounds, 
				 const GraphicsTools::Color &inColor);

	//////////
	// An optimized version of fill box for the case when the color
	// has a non-opaque alpha channel. Needs to be a separate function
	// so it can be in a separate, optimized file.
	//
	void FillBoxAlpha(const wxRect &inBounds,
					  const GraphicsTools::Color &inColor);

	//////////
	// Fill in the specified box with the specified color.
	//
	void OutlineBox(const wxRect &inBounds, const wxColour &inColor,
					int inWidth);

	// This might be a useful optimization, but we haven't implemented
	// it yet.  Feel free to remove the stubs if they linger.
#if 0
	//////////
	// Draw a GreyMap to the screen, colorizing it with inColor and
	// using the grey values as alpha.
	//
	void DrawGreyMap(GraphicsTools::Point inPoint,
					 GraphicsTools::GreyMap &inGreyMap,
					 GraphicsTools::Color inColor);
#endif // 0

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
	// Blit the contents of the specified DC to our offscreen buffer.
	// If the blit fails, fill the offscreen buffer with black.
	// (This is currently used for synchronizing our display with
	// whatever Quake 2 left on the screen.)
	// 
	// [in] inDC - a DC the same size as the stage
	//
	void DrawDCContents(wxDC &inDC);

	//////////
	// Save a portion of the current offscreen buffer to our save
	// buffer.
	// 
	// [in] inBounds - The rectangle defining the portion to save.
	//
	void SaveGraphics(const wxRect &inBounds);

	//////////
	// Restore a portion of the save buffer to our offscreen buffer.
	//
	// [in] inBounds - The rectangle defining the portion to restore.
	//
	void RestoreGraphics(const wxRect &inBounds);

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
	// Refresh the screen using the specified effect.
	//
	// [in] inTransition - The name of the transition to use, or "none".
	// [in] inMilliseconds - The desired duration of the transition.
	//
	void RefreshStage(const std::string &inTransition,
					  int inMilliseconds);

	//////////
	// Add a Element to this Stage.  This should only be called
	// by the Element class.
	//
	void AddElement(Element *inElement);

	//////////
	// Find an element by name.
	//
	// [in] inElementName - The name to search for.
	// [out] return - A pointer to the element, or NULL.
	//
	Element *FindElement(const wxString &inElementName);

	//////////
	// Find the lightweight Element containing the specified point, if
	// any.
	//
	// [in] inPoint - The point to check.
	// [out] return - A pointer to the Element, or NULL.
	//
	Element *FindLightWeightElement(const wxPoint &inPoint);

	//////////
	// Find the appropriate event dispatcher for the given point.
	// We normally call this function when we want to find a
	// lightweight element to handle some kind of mouse event.
	//
	// [in] inPoint - The point to check.
	// [out] return - The event dispatcher which should handle
	//                this event.
	//
	EventDispatcher *FindEventDispatcher(const wxPoint &inPoint);

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

	//////////
	// Return true if a movie is playing.
	//
	bool IsMoviePlaying();

	//////////
	// Delete all movie (audio & video) elements which are playing.
	//
	void DeleteMovieElements();

	//////////
	// "Grab" the mouse on behalf of the specified element.  This means
	// that all mouse events will be sent to that element until further
	// notice, regardless of where the event occurred.  Grabs are used to
	// implement standard buttons without busy-looping during mouse down.
	//
	void MouseGrab(Element *inElement);

	//////////
	// Ungrab the mouse.  'inElement' should match the previous grab.
	//
	void MouseUngrab(Element *inElement);

	//////////
	// Is the mouse grabbed right now?
	//
	bool MouseIsGrabbed() { return mGrabbedElement ? true : false; }

	//////////
	// Should we send mouse events to the specified element?  This is
	// normally true, unless a grab is in effect, in which case only
	// the grabbed element should receive mouse events.
	//
	bool ShouldSendMouseEventsToElement(Element *inElement);

    DECLARE_EVENT_TABLE();
};

#endif // Stage_H
