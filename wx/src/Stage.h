// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Stage_H
#define Stage_H

#include "AppGlobals.h"
#include "DirtyList.h"
#include "DrawingContextStack.h"

class StageFrame;
class Element;
typedef shared_ptr<Element> ElementPtr;
class MovieElement;
typedef shared_ptr<MovieElement> MovieElementPtr;
class EventDispatcher;
class ImageCache;
class CursorManager;
class TransitionManager;
class DrawingArea;

class Stage : public wxWindow
{
	//////////
	// A list of Elements.
	//
	typedef std::deque<ElementPtr> ElementCollection;

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
    // Before drawing graphics to the stage, we need to composite several
    // layers into a single image.  We use this buffer for that purpose.
    //
	wxBitmap mCompositingPixmap;

	//////////
	// Rectangles which need to be recomposited.
	//
	DirtyList mRectsToComposite;
    
    //////////
    // Rectangles which need to be redrawn to the screen during a refresh.
    //
    DirtyList mRectsToRefresh;

    //////////
    // This drawing area contains the background graphics for the stage.
	// Other layers are composited with this layer.
    //
	std::auto_ptr<DrawingArea> mBackgroundDrawingArea;

	//////////
	// The current stack of drawing contexts.
	//
	std::auto_ptr<DrawingContextStack> mDrawingContextStack;

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
	ElementPtr mCurrentElement;

	//////////
	// The element which has a "grab" on the mouse.
	//
	ElementPtr mGrabbedElement;

	//////////
	// The movie we're waiting on, or NULL if we're not waiting on anything.
	//
	MovieElementPtr mWaitElement;

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
	std::vector<wxPoint> mCopiedPoints;

	//////////
	// Get the compositing pixmap associated with this stage.
	//
	wxBitmap &GetCompositingPixmap();

	//////////
	// Get the background pixmap associated with this stage.
	//
	// TODO - Remove this method eventually and send all requests
	// directly to mBackgroundDrawingArea.
	//
	wxBitmap &GetBackgroundPixmap();

	//////////
	// Validate the entire stage--i.e., mark it as having been redrawn.
	//
	void ValidateStage();

	//////////
	// Repaint the stage.
	//
	void PaintStage(wxDC &inDC, const wxRegion &inDirtyRegion);

    //////////
    // Draw a border for the specified element.
    //
    void DrawElementBorder(wxDC &inDC, ElementPtr inElement);

	//////////
	// Draw a border for the text input control, if it exists.
	// 
	void Stage::DrawTextBorder(wxDC &inDC);

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
	void DestroyElement(ElementPtr inElement);

	//////////
	// We've entered an element; update things appropriately.
	//
	void EnterElement(ElementPtr inElement, wxPoint &inPosition);

	//////////
	// We've left an element; update things appropriately.
	//
	void LeaveElement(ElementPtr inElement, wxPoint &inPosition);

	//////////
	// Figure out which element we're inside, and figure out what cursor
	// we should be displaying now.
	//
	void UpdateCurrentElementAndCursor(wxPoint &inPosition);
	void UpdateCurrentElementAndCursor();

    //////////
    // Update the TStateDB system clock.
    //
    void UpdateClock();

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
	// Should the script be allowed to idle right now?
	//
	bool IsIdleAllowed() const;

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
    void NotifyEnterCard(const wxString &inName);

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
	// Redirect all further drawing calls to the specified element until
	// further notice.
	//
	void PushDrawingContext(ElementPtr inElement)
		{ mDrawingContextStack->PushDrawingContext(inElement); }

	//////////
	// Pop the top element off the current drawing stack, and make sure
	// it matches the specified element.
	//
	void PopDrawingContext(ElementPtr inElement)
		{ mDrawingContextStack->PopDrawingContext(inElement); }
	
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
		{ InvalidateScreen(); mIsDisplayingGrid = !mIsDisplayingGrid; }

    //////////
    // Are we currently displaying the borders?
    //
    bool IsDisplayingBorders() { return mIsDisplayingBorders; }

    //////////
    // Toggle the display of the borders.
    //
    void ToggleDisplayBorders()
		{ InvalidateScreen(); mIsDisplayingBorders = !mIsDisplayingBorders; }

	//////////
	// Invalidate the entire stage.
	//
	void InvalidateStage();

	//////////
	// Invalidate just the screen, not the offscreen compositing for the stage.
	//
	void InvalidateScreen();

	//////////
	// Invalidate the specified rectangle.
	//
	void InvalidateRect(const wxRect &inRect);

	//////////
	// Get the currently selected drawing area for this stage.
	//
	DrawingArea *GetCurrentDrawingArea();

	//////////
	// Get the background drawing area for this stage.
	//
	DrawingArea *GetBackgroundDrawingArea()
		{ return mBackgroundDrawingArea.get(); }

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
	// Save a screenshot to the specified file
	//
	// [in] inFilename - The name of the file to save to.
	//
	void Screenshot(const wxString &inFilename);

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
	void AddElement(ElementPtr inElement);

	//////////
	// Find an element by name.
	//
	// [in] inElementName - The name to search for.
	// [out] return - A pointer to the element, or NULL.
	//
	ElementPtr FindElement(const wxString &inElementName);

	//////////
	// Find the lightweight Element containing the specified point, if
	// any.
	//
	// [in] inPoint - The point to check.
	// [out] return - A pointer to the Element, or NULL.
	//
	ElementPtr FindLightWeightElement(const wxPoint &inPoint);

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
	bool IsMediaPlaying();

	//////////
	// End all media (audio & video) elements which are playing.
	//
	void EndMediaElements();

	//////////
	// "Grab" the mouse on behalf of the specified element.  This means
	// that all mouse events will be sent to that element until further
	// notice, regardless of where the event occurred.  Grabs are used to
	// implement standard buttons without busy-looping during mouse down.
	//
	void MouseGrab(ElementPtr inElement);

	//////////
	// Ungrab the mouse.  'inElement' should match the previous grab.
	//
	void MouseUngrab(ElementPtr inElement);

	//////////
	// Is the mouse grabbed right now?
	//
	bool MouseIsGrabbed() { return mGrabbedElement ? true : false; }

	//////////
	// Is the mouse grabbed by the specified element?
	//
	bool MouseIsGrabbedBy(ElementPtr inElement)
    	{ return mGrabbedElement == inElement; }

	//////////
	// Should we send mouse events to the specified element?  This is
	// normally true, unless a grab is in effect, in which case only
	// the grabbed element should receive mouse events.
	//
	bool ShouldSendMouseEventsToElement(ElementPtr inElement);

    DECLARE_EVENT_TABLE();
};

#endif // Stage_H
