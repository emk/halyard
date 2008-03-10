// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

#include "TamaleHeaders.h"

#include <wx/config.h>
#include <wx/filename.h>
#include <wx/clipbrd.h>
#include <wx/image.h>

#include "TInterpreter.h"
#include "TStyleSheet.h"
#include "TVersion.h"
#include "CrashReporter.h"
#include "doc/Document.h"
#include "doc/UserProgram.h"

#include "AppConfig.h"
#include "AppGlobals.h"
#include "AppGraphics.h"
#include "FiveLApp.h"
#include "Stage.h"
#include "StageAccessible.h"
#include "StageFrame.h"
#include "ProgramTree.h"
#include "Element.h"
#include "MovieElement.h"
#include "LocationBox.h"
#include "EventDispatcher.h"
#include "ImageCache.h"
#include "CursorManager.h"
#include "CursorElement.h"
#include "TStateListenerManager.h"
#include "Transition.h"
#include "DrawingArea.h"
#include "MediaElement.h"

#if CONFIG_HAVE_QUAKE2
#	include "Quake2Engine.h"
#endif // CONFIG_HAVE_QUAKE2

#define IDLE_INTERVAL (1000.0/FRAMES_PER_SECOND) // milliseconds

using namespace Halyard;


//=========================================================================
//  Stage Methods
//=========================================================================

BEGIN_EVENT_TABLE(Stage, wxWindow)
	EVT_TIMER(FIVEL_STAGE_TIMER, Stage::OnTimer)
    EVT_MOTION(Stage::OnMouseMove)
    EVT_ERASE_BACKGROUND(Stage::OnEraseBackground)
    EVT_PAINT(Stage::OnPaint)
    EVT_CHAR(Stage::OnChar)
	EVT_LEFT_DOWN(Stage::OnLeftDown)
	EVT_LEFT_DCLICK(Stage::OnLeftDClick)
	EVT_LEFT_UP(Stage::OnLeftUp)
	EVT_RIGHT_DOWN(Stage::OnRightDown)
	EVT_RIGHT_DCLICK(Stage::OnRightDown)
    EVT_MOUSE_CAPTURE_CHANGED(Stage::OnMouseCaptureChanged)
END_EVENT_TABLE()

Stage::Stage(wxWindow *inParent, StageFrame *inFrame, wxSize inStageSize)
    : wxWindow(), // Must use empty constructor; see below.
      mFrame(inFrame), mStageSize(inStageSize), mLastCard(""),
	  mCompositingPixmap(inStageSize.GetWidth(),
						 inStageSize.GetHeight(), 24),
	  mOffscreenFadePixmap(inStageSize.GetWidth(),
						   inStageSize.GetHeight(), 24),
	  mDesiredCursor(NULL), mActualCursor(NULL), mNeedToWakeUp(false),
      mElementsHaveChanged(false),
      mShouldHideCursorUntilMouseMoved(false),
      mIsDisplayingXy(false), mIsDisplayingGrid(false),
      mIsDisplayingBorders(false), mIsErrortraceCompileEnabled(false), 
      mIsBeingDestroyed(false)
{
    // Set up our class options, *then* call Create(), to avoid early
    // repainting of this window with the wrong options.  (Theoretically,
    // if we set wxBG_STYLE_CUSTOM, we don't need to override
    // EVT_ERASE_BACKGROUND and throw away the message.)
    //
    // Note that we *can't* use use wxCLIP_CHILDREN here, because some of
    // our children (in particular, movies) *need* to be overdrawn until
    // they're ready to draw themselves.
    SetBackgroundStyle(wxBG_STYLE_CUSTOM);
    SetBackgroundColour(STAGE_COLOR);
    Create(inParent, -1, wxDefaultPosition, inStageSize);

    // Set the owner of our mTimer object.
    mTimer.SetOwner(this, FIVEL_STAGE_TIMER);

	mBackgroundDrawingArea = 
		std::auto_ptr<DrawingArea>(new DrawingArea(this,
												   inStageSize.GetWidth(),
												   inStageSize.GetHeight(),
												   false));
	mDrawingContextStack =
		std::auto_ptr<DrawingContextStack>(new DrawingContextStack(this));
    GetBackgroundDrawingArea()->Clear();
    
	mLastIdleEvent = ::wxGetLocalTimeMillis();
	mEventDispatcher = new EventDispatcher();
	mImageCache = new ImageCache();
	mCursorManager = new CursorManager();
	mTransitionManager = new TransitionManager();

    // Install our custom accessibility handler.
    SetAccessible(new StageAccessible(this));
	
    // Initialize the clock.
    UpdateClock();

    // Send a timer event periodically.  Right now, we send two timer
    // events per IDLE_INTERVAL, in an effort to get half-frame accuracy
    // for waking up from a WAIT.
    mTimer.Start(IDLE_INTERVAL / 2, wxTIMER_CONTINUOUS);

	wxLogTrace(TRACE_STAGE_DRAWING, "Stage created.");
}

Stage::~Stage()
{
	mIsBeingDestroyed = true;
    mTimer.Stop();
	DeleteElements();

    // Destory various resources *after* all elements.
	delete mImageCache;
	delete mCursorManager; 
	delete mEventDispatcher;
	delete mTransitionManager;
	wxLogTrace(TRACE_STAGE_DRAWING, "Stage deleted.");
}

wxBitmap &Stage::GetCompositingPixmap() {
	// Make sure our compositing is up to date.
	if (!mRectsToComposite.empty()) {
		wxMemoryDC dc;
		dc.SelectObject(mCompositingPixmap);
		DirtyList::iterator dirty_i = mRectsToComposite.begin();
		wxLogTrace(TRACE_STAGE_DRAWING, "Begin compositing.");
		for (; dirty_i != mRectsToComposite.end(); ++dirty_i) {
			GetBackgroundDrawingArea()->CompositeInto(dc, *dirty_i);

            // Composite elements in two passes: regular elements first,
            // and then elements in the drag layer.
			ElementCollection::iterator elem_begin = mElements.begin();
            ElementCollection::iterator elem_end = mElements.end();
            ElementCollection::iterator elem_i;
			for (elem_i = elem_begin; elem_i != elem_end; ++elem_i)
                if (!(*elem_i)->IsInDragLayer())
                    (*elem_i)->CompositeInto(dc, *dirty_i);
			for (elem_i = elem_begin; elem_i != elem_end; ++elem_i)
                if ((*elem_i)->IsInDragLayer())
                    (*elem_i)->CompositeInto(dc, *dirty_i);
		}
		wxLogTrace(TRACE_STAGE_DRAWING, "End compositing.");
		mRectsToComposite.clear();
	}
	return mCompositingPixmap;
}

wxBitmap &Stage::GetBackgroundPixmap() {
	return mBackgroundDrawingArea->GetPixmap();
}

DrawingArea *Stage::GetCurrentDrawingArea() {
	return mDrawingContextStack->GetCurrentDrawingArea();
}

wxBitmap Stage::GetScriptGraphic(const std::string &inName) {
    FileSystem::Path path =
        FileSystem::GetScriptGraphicFilePath(inName);
    if (!path.DoesExist() || !path.IsRegularFile())
        return wxBitmap();
    std::string native_path = path.ToNativePathString();
    return GetImageCache()->GetBitmap(native_path.c_str());
}

void Stage::MaybeShowSplashScreen() {
    if (TInterpreterManager::ShouldSuppressSplashScreen())
        return;

    // TODO - We assume the bitmap is 800x450 pixels, and we lay out
    // this screen using hard-coded co-ordinates.

    // If we have a splash.png file, draw it onto the stage.
    MaybeDrawSplashGraphic("splash.png");

    // Get our copyright strings.
    UserProgram *prog = mFrame->GetDocument()->GetUserProgram();
    std::string script_copyright =
        prog->GetName() + ". " + prog->GetCopyright();
    std::string tamale_copyright =
        std::string(TAMALE_COPYRIGHT_NAME) + ". " + TAMALE_COPYRIGHT_NOTICE;

    // Now, set up a drawing context for our text.  We use wxWidgets to
    // draw the text because our font system won't have any text styles
    // loaded yet.
    wxMemoryDC dc;
    dc.SelectObject(GetBackgroundPixmap());
    
    // Prepare to draw the text.
    dc.SetTextForeground(*wxWHITE);
    dc.SetTextBackground(*wxBLACK);
    dc.SetFont(*wxNORMAL_FONT);
        
    // Draw the text.
    dc.DrawText(script_copyright.c_str(), 5, 515);
    dc.DrawText(tamale_copyright.c_str(), 5, 530);
    InvalidateRect(wxRect(0, 500, 800, 100));
}

void Stage::MaybeDrawSplashGraphic(const std::string &inName) {
    if (TInterpreterManager::ShouldSuppressSplashScreen())
        return;

    // TODO - We assume the bitmap is 800x450 pixels, and we lay out
    // this screen using hard-coded co-ordinates.
    wxBitmap bitmap = GetScriptGraphic(inName);
    if (bitmap.Ok())
        mBackgroundDrawingArea->DrawBitmap(bitmap, 0, 60);
}

void Stage::DrawLoadProgress() {
    if (TInterpreterManager::ShouldSuppressSplashScreen())
        return;

    // TODO - We lay out this screen using hard-coded co-ordinates.
    const int y_pos       = 459;
    const int x_begin     = 290;
    const int x_space_end = 510;
    const int x_space     = x_space_end - x_begin;

    // Figure out how much of our bar to actually draw.
    double frac = TInterpreter::GetInstance()->GetLoadProgress();
    int x_end = x_begin + x_space * frac;
    
    // Load our image, and use it to draw the progress bar.
    wxBitmap bitmap = GetScriptGraphic("progress.png");
    if (bitmap.Ok()) {
        for (int x = x_begin; x < x_end; ++x)
            mBackgroundDrawingArea->DrawBitmap(bitmap, x, y_pos);
    }
}

void Stage::RefreshSplashScreen() {
    // This is just a quick hack so that we don't repaint the stage when
    // scripters are running in development mode, and they want to visually
    // compare the before-and-after-refresh views of the stage.
    if (TInterpreterManager::ShouldSuppressSplashScreen())
        return;
    RefreshStage("none", 0);
}

void Stage::RaiseToTop(ElementPtr inElem) {
    // Move inElem to the end of our elements list.
    ElementCollection::iterator found =
        std::find(mElements.begin(), mElements.end(), inElem);
    ASSERT(found != mElements.end());
    mElements.erase(found);
    mElements.push_back(inElem);

    // If there's a DrawingArea attached to this element, we're going to
    // have to recomposite it (in case it has moved above or below another
    // element).  Note that this won't actually do anything if the Element
    // is being displayed over Quake 2.  See also Overlay::SetInDragLayer.
	DrawingArea *drawing_area = inElem->GetDrawingArea();
    if (drawing_area)
        drawing_area->InvalidateCompositing();
}

bool Stage::IsIdleAllowed() const {
	// Don't allow idling when we've got drawing contexts pushed.
	return mDrawingContextStack->IsEmpty();
}

bool Stage::IsScriptInitialized()
{
	// Assume that the script is properly initialized as soon as it has
	// entered a card.  This is good enough for now, but we'll need to
	// change it later.
	return (mLastCard != "");
}

void Stage::SetEditMode(bool inWantEditMode)
{
	if (IsInEditMode() == inWantEditMode)
		return;
	else if (inWantEditMode)
	{
		TInterpreter::GetInstance()->Stop();
		// TODO - NotifyExitCard() should be triggered from kernel.ss, but
		// this will mean auditing the engine code to only call
		// CurCardName, etc., only when there is a current card.
        //
        // In general, the whole edit-mode system is a pretty ugly kludge,
        // and much more of it should be handled from the interpreter.
		NotifyExitCard();
        // Delete any non-card elements.
        DeleteElements();
        // Delete any non-card, non-element listeners.
        gStateListenerManager.NotifyInterpreterStopped();
		GetBackgroundDrawingArea()->Clear();
	}
	else
	{
		wxASSERT(mLastCard != "");
        // TODO - We don't recreate group- or sequence-level elements
        // properly here.
		TInterpreter::GetInstance()->Go(mLastCard.c_str());
	}
}

bool Stage::IsInEditMode()
{
	wxASSERT(TInterpreter::HaveInstance());
	return TInterpreter::GetInstance()->IsStopped();
}

void Stage::HideCursorUntilMouseMoved() {
    mShouldHideCursorUntilMouseMoved = true;
}

bool Stage::ShouldShowCursor() {
    ASSERT(!mIsBeingDestroyed);

    // Handle the easy cases first.
    if (mShouldHideCursorUntilMouseMoved)
        return false;
    if (!IsScriptInitialized()
        || !mFrame->IsFullScreen()
        || mIsDisplayingXy
        || IsInEditMode()
        || mGrabbedElement)
        return true;

    // See if any of our elements want a cursor.
    ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); i++)
        if ((*i)->IsShown() && (*i)->WantsCursor())
            return true;

    // By default, we want to hide it.
    return false;
}

bool Stage::ShouldSendEvents()
{
	return (TInterpreter::HaveInstance() && IsScriptInitialized() &&
            !IsInEditMode());
}

bool Stage::CanJump()
{
	// This should match the list of sanity-checks in the function below.
	return (TInterpreter::HaveInstance() &&
			IsScriptInitialized() &&
			!IsInEditMode());
}

void Stage::TryJumpTo(const wxString &inName)
{
	// We go to quite a lot of trouble to verify this request.
	if (!IsScriptInitialized())
		::wxLogError("Cannot jump until program has finished initializing.");
	else if (IsInEditMode())
		::wxLogError("Unimplemented: Cannot jump while in edit mode (yet).");
	else
	{
		wxASSERT(TInterpreter::HaveInstance());
		TInterpreter *interp = TInterpreter::GetInstance();
		if (!interp->IsValidCard(inName))
			::wxLogError("The card \'" + inName + "\' does not exist.");
		else
			interp->JumpToCardByName(inName);
	}
}

void Stage::RegisterCard(const wxString &inName)
{
	mFrame->GetProgramTree()->RegisterCard(inName);
}

void Stage::NotifyEnterCard(const wxString &inName)
{
	mLastCard = inName;
	mFrame->GetLocationBox()->NotifyEnterCard(inName);
	mFrame->GetProgramTree()->NotifyEnterCard(inName);
    CrashReporter::GetInstance()->SetCurrentCard(inName.mb_str());

    // If the script is waiting on a media element, end the wait now.
    if (mWaitElement)
        EndWait();
}

void Stage::NotifyExitCard()
{
    // Do nothing.
}

void Stage::NotifyReloadScriptStarting()
{
	mLastCard = "";
    NotifyExitCard();
	DeleteElements();
	gStyleSheetManager.RemoveAll();
}

void Stage::NotifyElementsChanged()
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Elements on stage have changed.");
    // Notify our OnTimer method that the elements on the stage have
    // changed, and that we will need to recalculate the current element.
    // We can't do any of that work _here_, because recalculating the
    // current element may generate mouse-enter/mouse-leave events, which
    // we should only send at well-defined times (such as during event
    // processing in OnTimer).
    mElementsHaveChanged = true;
}

void Stage::EnterElement(ElementPtr inElement, wxPoint &inPosition)
{
    if (ShouldSendEvents()) {
        ASSERT(inElement->GetEventDispatcher().get());
        inElement->GetEventDispatcher()->DoEventMouseEnter(inPosition);
    }
}

void Stage::LeaveElement(ElementPtr inElement, wxPoint &inPosition)
{
    if (ShouldSendEvents()) {
        ASSERT(inElement->GetEventDispatcher().get());
        inElement->GetEventDispatcher()->DoEventMouseLeave(inPosition);
    }
}

wxPoint Stage::CurrentMousePosition() {
    return ScreenToClient(::wxGetMousePosition());
}

void Stage::UpdateCurrentElementAndCursor(wxPoint &inPosition)
{
    // Performance Note: UpdateCurrentElementAndCursor() is called
    // very often (on every mouse move, plus other places). There are
    // two potentially expensive things it does; it does a linear
    // search of all elements on the screen, and it does a log(n)
    // search of all registered cursors. If it becomes too slow, 
    // check to see if either of those can be optimized.

    // Find which element we're in.
	ElementPtr obj = FindLightWeightElement(inPosition);

	// Change the cursor, if necessary.  I haven't refactored this
	// into EnterElement/LeaveElement yet because of how we handle
	// mIsDisplayingXy.  Feel free to improve.
	if (!mGrabbedElement)
	{
		if (mIsDisplayingXy)
			mDesiredCursor = mCursorManager->FindCursor("cross");
		else
		{
			if (obj) {
                std::string name = obj->GetCursorName();
				mDesiredCursor = mCursorManager->FindCursor(name);
            } else {
				mDesiredCursor = mCursorManager->FindCursor("arrow");
            }
		}
	}

    // We *always* call this function, because the result of
    // ShouldShowCursor() may have changed.
    UpdateDisplayedCursor();

	// Update the current element.
	if (obj != mCurrentElement)
	{
		if (mCurrentElement && ShouldSendMouseEventsToElement(mCurrentElement))
			LeaveElement(mCurrentElement, inPosition);
		mCurrentElement = obj;
		if (obj && ShouldSendMouseEventsToElement(obj))
			EnterElement(obj, inPosition);
	}
}

void Stage::UpdateCurrentElementAndCursor() {
	UpdateCurrentElementAndCursor(CurrentMousePosition());
}

void Stage::UpdateDisplayedCursor() {
    Cursor *cursor(mDesiredCursor);
    if (!ShouldShowCursor())
        cursor = mCursorManager->FindCursor("blank");

    if (mActualCursor != cursor) {
        if (mActualCursor)
            mActualCursor->UnsetStageCursor();

        if (cursor) {
            cursor->SetStageCursor(CurrentMousePosition());
        } else {
            SetCursor(wxNullCursor);
        }
        mActualCursor = cursor;
    }
}

void Stage::ReplaceDisplayedCursorWithDefault() {
    // TODO - Default element cursor is "hand" here, in Element.h
    // and in CursorManager.cpp.  Refactor to one place.
    Cursor *cursor(mCursorManager->FindCursor("hand"));
    mDesiredCursor = cursor;

    // Calling ShouldShowCursor will crash if our Stage is being destoyed.
    if (!mIsBeingDestroyed && !ShouldShowCursor())
        cursor = mCursorManager->FindCursor("blank");

    // We can't call UnsetStageCursor here, because we don't want to give
    // the user's script code the chance to call us re-entrantly.
    cursor->SetStageCursor(CurrentMousePosition());
    mActualCursor = cursor;
}

void Stage::UpdateClock() {
    // Set our two clock variables.  Note that this may cause script code
    // to run in response to the updates.
	// XXX - Do something more accurate than GetLo with the milliseconds.
    gStateDB.Set("/system/clock/seconds", ::wxGetLocalTime());
    gStateDB.Set("/system/clock/milliseconds",
				 ::wxGetLocalTimeMillis().GetLo());
}

/// Put our interpreter to sleep.  It's our caller's responsibility to make
/// sure TInterpreter::CanSuspend() is currently true.
void Stage::InterpreterSleep()
{
	// TODO - Keep track of who we're sleeping for.
    ASSERT(TInterpreter::HaveInstance() &&
           !TInterpreter::GetInstance()->Paused() &&
           TInterpreter::GetInstance()->CanSuspend());
    TInterpreter::GetInstance()->Pause();
}

/// Wake up our Scheme interpreter.
void Stage::InterpreterWakeUp()
{
	// We can't check TInterpreter::GetInstance()->Paused() because
	// the engine might have already woken the script up on its own.
	// TODO - Keep track of who we're sleeping for.
    ASSERT(TInterpreter::HaveInstance());
    TInterpreter::GetInstance()->WakeUp();
    gDebugLog.Log("wait: Finished waking up");
}

void Stage::InterpreterWakeUpIfNecessary() {
    if (mNeedToWakeUp) {
        mNeedToWakeUp = false;
        if (TInterpreter::HaveInstance())
            InterpreterWakeUp();
    }
}

Stage::ElementCollection::iterator
Stage::FindElementByName(ElementCollection &inCollection,
						 const wxString &inName)
{
	ElementCollection::iterator i = inCollection.begin();
	for (; i != inCollection.end(); i++)
		if ((*i)->GetName() == inName)
			return i;
	return inCollection.end();
}

void Stage::IdleElements() {
    // Send idle events to all our elements.  We use a copy of
    // mElements in case elements delete themselves in their idle
    // function.
    ElementCollection elems = mElements;
    ElementCollection::iterator i = elems.begin();
    for (; i != elems.end(); i++)
        /// \bug *i may have been deleted by *someone else* earlier in idle
        /// processing.  The state-db contains code to handle similar
        /// problems, but for now, we're going to hope nobody does this.
        (*i)->Idle();
}

void Stage::OnTimer(wxTimerEvent& inEvent)
{
	ASSERT(!mIsBeingDestroyed);

    // If any elements have changed on the stage since we last encountered
    // this loop, then we need to call UpdateCurrentElementAndCursor.
	if (mElementsHaveChanged) {
        mElementsHaveChanged = false;
		// Update our element borders (if necessary) and fix our cursor.
		if (mIsDisplayingBorders)
			InvalidateScreen();
		UpdateCurrentElementAndCursor();
	}

    // Check our displayed cursor, because the return value of
    // ShouldShowCursor() might have changed.
    UpdateDisplayedCursor();

    // If we've reached the end of our current WAIT, end it.
	if (mWaitElement && mWaitElement->HasReachedFrame(mWaitFrame))
		EndWait();

    // We used to call InterpreterWakeUpIfNecessary here (and also in
    // Stage::NotifyCardEntered), because it was illegal to call
    // InterpreterWakeUp from a callback. But we now have a supposedly
    // better system, involving interpreter calls to WakeUpIfNecessary.  So
    // we don't do call InterpreterWakeUpIfNecessary here, because even
    // though it was perfectly safe and legal, it wasn't sufficient.

	// Send an idle event to the Scheme engine occasionally.
	if (ShouldSendEvents() &&
		::wxGetLocalTimeMillis() > mLastIdleEvent + IDLE_INTERVAL)
	{
		mLastIdleEvent = ::wxGetLocalTimeMillis();

        // Now's an excellent time to update the clock information.
        UpdateClock();

        // Send idle events to all our elements.
        IdleElements();

		// We only pass the idle event to just the card, and not any
		// of the elements.  Idle event processing is handled differently
		// from most other events; we let the scripting language work
		// out the details.
		GetEventDispatcher()->DoEventIdle();
	}
}

void Stage::OnMouseMove(wxMouseEvent &inEvent)
{
	// This function occasionally gets called when destroying
	// broken QuickTime movies.
	if (mIsBeingDestroyed)
		return;

	// Do any mouse-moved processing for our Elements.
    mShouldHideCursorUntilMouseMoved = false;
	UpdateCurrentElementAndCursor();

    // Notify our cursor of its new location.
    if (mActualCursor)
        mActualCursor->MoveCursor(inEvent.GetPosition());

    // If we're displaying XY co-ordinates for the cursor, update them now.
    if (mIsDisplayingXy)
    {
        wxClientDC dc(this);

        // Get our current screen location.
        wxPoint pos = inEvent.GetPosition();
        long x = dc.DeviceToLogicalX(pos.x);
        long y = dc.DeviceToLogicalY(pos.y);

        // Get the color at that screen location.
        // PORTING - May not work on non-Windows platforms, according to
        // the wxWindows documentation.
        wxMemoryDC offscreen_dc;
        offscreen_dc.SelectObject(GetCompositingPixmap());
        wxColour color;
        offscreen_dc.GetPixel(x, y, &color);

        // Update the status bar.
        wxString str;
        str.Printf("X: %d, Y: %d, C: %02X%02X%02X",
                   (int) x, (int) y, (int) color.Red(),
                   (int) color.Green(), color.Blue());
        mFrame->SetStatusText(str);
    }

	if (ShouldSendEvents())
	{
		EventDispatcher *disp = FindEventDispatcher(inEvent.GetPosition());
		disp->DoEventMouseMoved(inEvent);
	}
}

void Stage::OnEraseBackground(wxEraseEvent &inEvent)
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Ignoring request to erase stage.");

    // Ignore this event to prevent flicker--we don't need to erase,
    // because we redraw everything from the offscreen buffer.  We may need
    // to override more of these events elsewhere.

	// TODO - Sometimes parts of the frame don't get repainted.  Could we
	// somehow indirectly be responsible?
}

void Stage::OnPaint(wxPaintEvent &inEvent)
{
	if (mIsBeingDestroyed)
		return;

	wxLogTrace(TRACE_STAGE_DRAWING, "Painting stage.");

    // Set up our drawing context, and paint the screen.
    wxPaintDC screen_dc(this);
    PaintStage(screen_dc, GetUpdateRegion());
}

void Stage::ClipElementsThatDrawThemselves(wxDC &inDC) 
{
    // Clip our heavyweight elements, so that we don't attempt to redraw
    // the stage over movies or edit boxes.  Note that we don't use
    // wxCLIP_CHILDREN here, because some of our children (particularly
    // movies) *need* to be overdrawn until they're ready to draw
    // themselves.
    bool need_clipping = false;
    wxRegion clip_to(wxRect(wxPoint(0, 0), GetSize()));
    ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); ++i)
        if ((*i)->IsShown() && (*i)->ApplyClippingToStage(clip_to))
            need_clipping = true;

    // If we actually made any changes to our clipping region, apply it.
    // The SetClippingRegion function actually calculates the intersection
    // of the old clipping region and the new one, and uses _that_.
    // (Remember, we get a new wxDC every time we need to repaint, and the
    // operating system may attempt to set up a reasonable clipping region
    // for us.)
    if (need_clipping)
        inDC.SetClippingRegion(clip_to);
}

void Stage::PaintStage(wxDC &inDC, const wxRegion &inDirtyRegion)
{
	// Make sure we don't overdraw any heavyweight elements.
	ClipElementsThatDrawThemselves(inDC);

    // Blit our offscreen pixmap to the screen.
    {
        wxMemoryDC srcDC;
        srcDC.SelectObject(GetCompositingPixmap());
        wxRegionIterator i(inDirtyRegion);
        while (i) {
            inDC.Blit(i.GetX(), i.GetY(), i.GetW(), i.GetH(),
                      &srcDC, i.GetX(), i.GetY());
            i++;
        }
    }

    // If necessary, draw the grid.
    if (mIsDisplayingGrid)
    {
        int width = mStageSize.GetWidth();
        int height = mStageSize.GetHeight();
        int small_spacing = 10;
        int large_spacing = small_spacing * 10;

        // Draw the minor divisions of the grid.
        inDC.SetPen(*wxLIGHT_GREY_PEN);
        for (int x = 0; x < width; x += small_spacing)
            if (x % large_spacing)
                inDC.DrawLine(x, 0, x, height);
        for (int y = 0; y < width; y += small_spacing)
            if (y % large_spacing)
                inDC.DrawLine(0, y, width, y);

        // Draw the major divisions of the grid.
        inDC.SetPen(*wxGREEN_PEN);
        for (int x2 = 0; x2 < width; x2 += large_spacing)
            inDC.DrawLine(x2, 0, x2, height);
        for (int y2 = 0; y2 < width; y2 += large_spacing)
            inDC.DrawLine(0, y2, width, y2);
    }

	// If necessary, draw the borders.
	if (mIsDisplayingBorders)
	{
		ElementCollection::iterator i = mElements.begin();
		for (; i != mElements.end(); i++)
			if ((*i)->IsShown())
				DrawElementBorder(inDC, *i);
	}
}

// XXX - these should be refactored, but it's just two lines they have 
// in common. I'm not sure if it's worth it. Feel free to do so if you
// want.
void Stage::DrawElementBorder(wxDC &inDC, ElementPtr inElement)
{
    if (inElement->WantsCursor())
        inDC.SetPen(*wxRED_PEN);
    else
        inDC.SetPen(*wxGREY_PEN);
	inDC.SetBrush(*wxTRANSPARENT_BRUSH);

	inElement->DrawElementBorder(inDC);
}

void Stage::OnChar(wxKeyEvent &inEvent)
{
	// NOTE - We handle this event here, but the stage isn't always
	// focused.  Is this really a good idea?  Douglas tells me that
	// Director works like this, so at least there's precedent.
	if (!ShouldSendEvents())
		inEvent.Skip();
	else if (inEvent.GetKeyCode() == WXK_SPACE &&
			 inEvent.ControlDown() && !inEvent.AltDown())
		inEvent.Skip(); // Always allow toggling into edit mode.
	else
	{
		EventDispatcher *dispatcher = GetEventDispatcher();
		if (!dispatcher->DoEventChar(inEvent))
			inEvent.Skip();
	}
}

void Stage::OnLeftDown(wxMouseEvent &inEvent)
{
	// Restore focus to the stage (or our game engine, if it's on top).
	if (Quake2Engine::IsDisplayed())
		Quake2Engine::GetInstance()->SetFocus();
	else
		SetFocus();

	// Dispatch the event.
    if (ShouldSendEvents()) {
        EventDispatcher *disp = FindEventDispatcher(inEvent.GetPosition());
        disp->DoEventLeftDown(inEvent, false);
    }
}

void Stage::OnLeftDClick(wxMouseEvent &inEvent)
{
    if (ShouldSendEvents()) {
        EventDispatcher *disp = FindEventDispatcher(inEvent.GetPosition());
        disp->DoEventLeftDown(inEvent, true);	
    }
}

void Stage::OnLeftUp(wxMouseEvent &inEvent)
{
    if (ShouldSendEvents()) {
        EventDispatcher *disp = FindEventDispatcher(inEvent.GetPosition());
        disp->DoEventLeftUp(inEvent);
    }
}

void Stage::OnRightDown(wxMouseEvent &inEvent)
{
    if (!mIsDisplayingXy)
		inEvent.Skip();
	else
	{
		// Get the position of the click, build a string, and save the
		// position for next time.
		wxPoint pos = inEvent.GetPosition();
		wxString str;
		if (inEvent.ShiftDown() && mCopiedPoints.size() == 1)
			str.Printf("(rect %d %d %d %d)", 
					   (mCopiedPoints.end()-1)->x, 
					   (mCopiedPoints.end()-1)->y,
					   pos.x, pos.y);
		else if (inEvent.ShiftDown() && mCopiedPoints.size() > 1)
		{
			str.Printf("(polygon ");
			std::vector<wxPoint>::iterator i;
			for (i = mCopiedPoints.begin(); i != mCopiedPoints.end(); ++i)
				str += wxString::Format("(point %d %d) ", i->x, i->y);
			str += wxString::Format("(point %d %d))", pos.x, pos.y);
		}
		else
		{
			str.Printf("(point %d %d)", pos.x, pos.y);
			mCopiedPoints.clear();
		}
		mCopiedPoints.push_back(pos);

		CopyStringToClipboard(str);
	}
}

void Stage::OnMouseCaptureChanged(wxMouseCaptureChangedEvent &inEvent) {
    // According to the manual: "An mouse capture changed event is sent to
    // a window that loses its mouse capture."  Theoretically, this means
    // that we should *never* receive this event to notify us that we
    // *regained* a mouse capture.  However, there's a stack of mouse
    // captures in wxWidgets, and the event name is suspiciously
    // ambiguous, so we're going to check.
    ASSERT(inEvent.GetCapturedWindow() != this);

    // Clear the grabbed element.  Note that this code will be run whenever
    // we lose the mouse grab (at least on Windows, where this event is
    // implemented), even if it's because MouseUngrab called ReleaseMouse
    // directly.  So this may be redudant 99% of the time on windows.  But
    // the other 1% of the time, it happens because somebody popped up an
    // error window while the mouse was grabbed.
	mGrabbedElement = ElementPtr();
}

void Stage::CopyStringToClipboard(const wxString &inString)
{
	// Copy our string to the clipboard.  This code snippet comes from
	// the wxWindows manual.
	if (wxTheClipboard->Open())
	{
		wxTheClipboard->SetData(new wxTextDataObject(inString));
		wxTheClipboard->Close();
		mFrame->SetStatusText(wxString("Copied: ") + inString);
	}
}

void Stage::ValidateStage()
{
	// XXX - We can't actually *do* this using wxWindows, so we're
	// repainting the screen too often.  But this would be an excellent
    // time to clear mRectsToRefresh.
    mRectsToRefresh.clear();
}

void Stage::InvalidateStage()
{
    InvalidateRect(wxRect(0, 0,
                          mStageSize.GetWidth(), mStageSize.GetHeight()));
}

void Stage::InvalidateScreen() {
    // This is called when we want to invalidate just the stuff on the screen,
    // and not our offscreen compositing buffers.
    Refresh(FALSE);
}

void Stage::InvalidateRect(const wxRect &inRect)
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Invalidating: %d %d %d %d",
			   inRect.x, inRect.y, inRect.GetRight(), inRect.GetBottom());
    // It's a little bit inelegant to maintain two different dirty lists,
    // but they get cleared by different actions.
	mRectsToComposite.MergeRect(inRect);

    // Trigger screen repaint events--and update our manual refresh
    // list--but only if Quake 2 is not being displayed.  (Quake 2 covers
    // the entire stage, and if we repaint the screen, it will flicker.)
    // The entire screen will automatically be refreshed when Quake 2
    // is hidden.
    if (!Quake2Engine::IsDisplayed()) {
        mRectsToRefresh.MergeRect(inRect);
        Refresh(FALSE, &inRect);
    }
}

void Stage::Screenshot(const wxString &inFilename)
{
	wxImage image = GetCompositingPixmap().ConvertToImage();
	image.SaveFile(inFilename, wxBITMAP_TYPE_PNG);
}

bool Stage::Wait(const wxString &inElementName, MovieFrame inUntilFrame)
{
	ASSERT(!mWaitElement);
    ASSERT(!mNeedToWakeUp);

	// Look for our element.
	ElementCollection::iterator i =
		FindElementByName(mElements, inElementName);

	// Make sure we can wait on this element.
	// TODO - Refactor this error-handling code to a standalone
	// routine so we don't have to keep on typing it.  (Actually, this code
    // is handled nicely by some macros in TWxPrimitives.cpp, although they
    // report errors quite a bit more noisily.
	const char *name = (const char *) inElementName;
	if (i == mElements.end())
	{
		gDebugLog.Caution("wait: Element %s does not exist", name);
		return false;
	}
	MediaElementPtr media = MediaElementPtr(*i, dynamic_cast_tag());
	if (!media)
	{
		gDebugLog.Caution("wait: Element %s is not a media element", name);
		return false;		
	}

	// Return immediately (if we're already past the wait point) or
	// go to sleep for a while.
	if (media->HasReachedFrame(inUntilFrame))
		gDebugLog.Log("wait: Media element %s has already past frame %d",
					  name, inUntilFrame);
	else
	{
		mWaitElement = media;
		mWaitFrame = inUntilFrame;
		InterpreterSleep();
	}
	return true;
}

void Stage::EndWait()
{
	gDebugLog.Log("wait: Waking up.");
	ASSERT(mWaitElement.get());
	mWaitElement = MediaElementPtr();
	mWaitFrame = 0;
    mNeedToWakeUp = true;
}

void Stage::RefreshStage(const std::string &inTransition, int inMilliseconds)
{
	// If we're supposed to run a transiton, do so now.
	if (inTransition != "none" && inMilliseconds > 0)
	{
		// Attempt to get a copy of whatever is on the screen.
		wxClientDC client_dc(this);

		wxBitmap before(mStageSize.GetWidth(), mStageSize.GetHeight(), 24);
		bool have_before;
		{
			wxMemoryDC before_dc;
			before_dc.SelectObject(before);
			have_before =
				before_dc.Blit(0, 0,
							   mStageSize.GetWidth(), mStageSize.GetHeight(),
							   &client_dc, 0, 0);
		}

		// Run transiton, if we can.
		if (have_before)
		{
            // Calculate a single dirty rectangle for the transition.
            wxRect dirty = mRectsToRefresh.GetBounds();
            dirty.Intersect(wxRect(wxPoint(0, 0), mStageSize));

			// Make sure we don't overdraw any heavyweight elements.
			ClipElementsThatDrawThemselves(client_dc);

            // Run the transition itself.
			TransitionResources r(client_dc, before, GetCompositingPixmap(),
								  mOffscreenFadePixmap, dirty);
			mTransitionManager->RunTransition(inTransition, inMilliseconds, r);
		}
	}

	// Draw our offscreen buffer to the screen, and mark that portion of
	// the screen as updated.
	{
		wxClientDC client_dc(this);
		PaintStage(client_dc, mRectsToRefresh);
	}
	ValidateStage();
}

void Stage::AddElement(ElementPtr inElement)
{
	// Delete any existing Element with the same name.
	(void) DeleteElementByName(inElement->GetName());

	// Add the new Element to our list.
	mElements.push_back(inElement);
	NotifyElementsChanged();
}

ElementPtr Stage::FindElement(const wxString &inElementName)
{
	ElementCollection::iterator i =
		FindElementByName(mElements, inElementName);
	if (i == mElements.end())
		return ElementPtr();
	else
		return *i;
}

ElementPtr Stage::FindLightWeightElement(const wxPoint &inPoint)
{
	// Look for the most-recently-added Element containing inPoint.
	ElementPtr result;
	ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); i++)
		if ((*i)->IsLightWeight() && (*i)->IsShown() &&
            (*i)->WantsCursor() && (*i)->IsPointInElement(inPoint))
			result = *i;
	return result;
}

EventDispatcher *Stage::FindEventDispatcher(const wxPoint &inPoint)
{
	// If a grab is in effect, return the element immediately.
	if (mGrabbedElement)
		return mGrabbedElement->GetEventDispatcher().get();

	// Otherwise, look things up normally.
	ElementPtr elem = FindLightWeightElement(inPoint);
	if (elem && elem->GetEventDispatcher())
		return elem->GetEventDispatcher().get();
	else
		return GetEventDispatcher();
}

void Stage::DestroyElement(ElementPtr inElement)
{
	wxString name = inElement->GetName();

	// Make sure this element isn't on our drawing context stack.
	if (mDrawingContextStack->ContainsElement(inElement))
		gLog.FatalError("Tried to delete an element with an active drawing "
						"context");

	// Clean up any dangling references to this object.
	if (inElement == mGrabbedElement)
		MouseUngrab(mGrabbedElement);
	if (inElement == mCurrentElement)
		mCurrentElement = ElementPtr();
    MediaElementPtr as_media(inElement, dynamic_cast_tag());
	if (as_media && as_media == mWaitElement)
		EndWait();

    // If the element we're deleting is a CursorElement, then we need to
    // tell it to unregister itself, and then update the cursor we're
    // displaying in case it has changed.  This takes care of
    // mDesiredCursor and mActualCursor.
    shared_ptr<CursorElement> as_cursor_elem(inElement, dynamic_cast_tag());
    if (as_cursor_elem) {
        as_cursor_elem->UnregisterWithCursorManager(mCursorManager);

        Cursor *as_cursor(static_cast<Cursor*>(as_cursor_elem.get()));
        if (as_cursor == mDesiredCursor)
            ReplaceDisplayedCursorWithDefault();
        ASSERT(as_cursor != mDesiredCursor);
        ASSERT(as_cursor != mActualCursor);
    }

	// We don't have to destroy the object explicity, because the
	// ElementPtr smart-pointer class will take care of that for us.
    //
	// TODO - Implemented delayed destruction so element callbacks can
	// destroy the element they're attached to.
}

bool Stage::DeleteElementByName(const wxString &inName)
{
	bool found = false;
	ElementCollection::iterator i = FindElementByName(mElements, inName);
	if (i != mElements.end())
	{
		// Completely remove from the collection first, then destroy.
		ElementPtr elem = *i;
		mElements.erase(i);
		DestroyElement(elem);
		found = true;
	}
	NotifyElementsChanged();
	return found;
}

void Stage::DeleteElements()
{
	ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); ++i)
		DestroyElement(*i);
	mElements.clear();
	NotifyElementsChanged();
}

bool Stage::IsMediaPlaying()
{
	ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); ++i) {
		MediaElementPtr elem = MediaElementPtr(*i, dynamic_cast_tag());
		if (elem && !elem->IsLooping())
			return true;
    }
	return false;
}

void Stage::EndMediaElements()
{
	ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); ++i) {
		MediaElementPtr elem = MediaElementPtr(*i, dynamic_cast_tag());
		if (elem && !elem->IsLooping()) {
			gDebugLog.Log("Manually ending media: %s",
						  (*i)->GetName().mb_str());
			elem->EndPlayback();
		}
	}
}

void Stage::MouseGrab(ElementPtr inElement)
{
	ASSERT(inElement->IsLightWeight());
	ASSERT(inElement->GetEventDispatcher().get());
	if (mGrabbedElement)
	{
		gLog.Error("Grabbing %s while %s is already grabbed",
				   inElement->GetName().mb_str(),
				   mGrabbedElement->GetName().mb_str());
		MouseUngrab(mGrabbedElement);
	}
	mGrabbedElement = inElement;
	CaptureMouse();
}

void Stage::MouseUngrab(ElementPtr inElement)
{
	ASSERT(inElement->IsLightWeight());
	if (!mGrabbedElement)
	{
		gLog.Error("Ungrabbing %s when it isn't grabbed",
				   inElement->GetName().mb_str());
		return;
	}
	if (inElement != mGrabbedElement)
	{
		gLog.Error("Ungrabbing %s when %s is grabbed",
				   inElement->GetName().mb_str(),
				   mGrabbedElement->GetName().mb_str());
	}

	// Force updating of the current element, cursor, etc.
	if (mCurrentElement != mGrabbedElement)
		mCurrentElement = ElementPtr();

	// Release our grab.
	mGrabbedElement = ElementPtr();
	ReleaseMouse();
	UpdateCurrentElementAndCursor();
}

bool Stage::ShouldSendMouseEventsToElement(ElementPtr inElement)
{
	return !mGrabbedElement || (inElement == mGrabbedElement);
}
