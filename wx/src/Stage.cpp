// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include <wx/config.h>
#include <wx/filename.h>
#include <wx/clipbrd.h>
#include <wx/image.h>

#include "TInterpreter.h"
#include "TVariable.h"
#include "TStyleSheet.h"

#include "AppConfig.h"
#include "AppGlobals.h"
#include "AppGraphics.h"
#include "FiveLApp.h"
#include "Stage.h"
#include "StageFrame.h"
#include "ProgramTree.h"
#include "Element.h"
#include "MovieElement.h"
#include "LocationBox.h"
#include "EventDispatcher.h"
#include "ImageCache.h"
#include "CursorManager.h"
#include "Transition.h"
#include "DrawingArea.h"

#if CONFIG_HAVE_QUAKE2
#	include "Quake2Engine.h"
#endif // CONFIG_HAVE_QUAKE2

#define IDLE_INTERVAL (33) // milliseconds

USING_NAMESPACE_FIVEL


//=========================================================================
//  Stage Methods
//=========================================================================

BEGIN_EVENT_TABLE(Stage, wxWindow)
	EVT_IDLE(Stage::OnIdle)
    EVT_MOTION(Stage::OnMouseMove)
    EVT_ERASE_BACKGROUND(Stage::OnEraseBackground)
    EVT_PAINT(Stage::OnPaint)
    EVT_CHAR(Stage::OnChar)
    EVT_TEXT_ENTER(FIVEL_TEXT_ENTRY, Stage::OnTextEnter)
	EVT_LEFT_DOWN(Stage::OnLeftDown)
	EVT_LEFT_DCLICK(Stage::OnLeftDClick)
	EVT_LEFT_UP(Stage::OnLeftUp)
	EVT_RIGHT_DOWN(Stage::OnRightDown)
	EVT_RIGHT_DCLICK(Stage::OnRightDown)
END_EVENT_TABLE()

Stage::Stage(wxWindow *inParent, StageFrame *inFrame, wxSize inStageSize)
    : wxWindow(inParent, -1, wxDefaultPosition, inStageSize),
      mFrame(inFrame), mStageSize(inStageSize), mLastCard(""),
	  mCompositingPixmap(inStageSize.GetWidth(),
						 inStageSize.GetHeight(), 24),
	  mOffscreenFadePixmap(inStageSize.GetWidth(),
						   inStageSize.GetHeight(), 24),
	  mSavePixmap(inStageSize.GetWidth(), inStageSize.GetHeight(), 24),
	  mTextCtrl(NULL), mCurrentElement(NULL), mGrabbedElement(NULL),
	  mWaitElement(NULL),
      mIsDisplayingXy(false), mIsDisplayingGrid(false),
      mIsDisplayingBorders(false)
{
	mBackgroundDrawingArea = 
		std::auto_ptr<DrawingArea>(new DrawingArea(this,
												   inStageSize.GetWidth(),
												   inStageSize.GetHeight(),
												   false));
	mDrawingContextStack =
		std::auto_ptr<DrawingContextStack>(new DrawingContextStack(this));
	
    SetBackgroundColour(STAGE_COLOR);
    GetBackgroundDrawingArea()->Clear();
    
	mLastIdleEvent = ::wxGetLocalTimeMillis();
	mEventDispatcher = new EventDispatcher();
	mImageCache = new ImageCache();
	mCursorManager = new CursorManager();
	mTransitionManager = new TransitionManager();
	
    mTextCtrl =
        new wxTextCtrl(this, FIVEL_TEXT_ENTRY, "", wxDefaultPosition,
                       wxDefaultSize, wxNO_BORDER | wxTE_PROCESS_ENTER);
    mTextCtrl->Hide();

	wxLogTrace(TRACE_STAGE_DRAWING, "Stage created.");
}

Stage::~Stage()
{
	DeleteElements();
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
			ElementCollection::iterator elem_i = mElements.begin();
			for (; elem_i != mElements.end(); ++elem_i)
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
		NotifyExitCard();
		GetBackgroundDrawingArea()->Clear();
	}
	else
	{
		wxASSERT(mLastCard != "");
		TInterpreter::GetInstance()->Go(mLastCard.c_str());
	}
}

bool Stage::IsInEditMode()
{
	wxASSERT(TInterpreter::HaveInstance());
	return TInterpreter::GetInstance()->IsStopped();
}

bool Stage::ShouldSendEvents()
{
	return IsScriptInitialized() && !IsInEditMode();
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

void Stage::NotifyEnterCard()
{
	mLastCard = TInterpreter::GetInstance()->CurCardName();
	mFrame->GetLocationBox()->NotifyEnterCard();
	mFrame->GetProgramTree()->NotifyEnterCard();
}

void Stage::NotifyExitCard()
{
    if (mTextCtrl->IsShown())
        mTextCtrl->Hide();
}

void Stage::NotifyScriptReload()
{
	mLastCard = "";
	mEventDispatcher->NotifyScriptReload();
	mImageCache->NotifyScriptReload();
	mFrame->GetProgramTree()->NotifyScriptReload();
    NotifyExitCard();
	DeleteElements();
	gStyleSheetManager.RemoveAll();

	// For now, treat Quake 2 as a special case.
#if CONFIG_HAVE_QUAKE2
	if (Quake2Engine::IsInitialized())
		Quake2Engine::GetInstance()->NotifyScriptReload();
#endif // CONFIG_HAVE_QUAKE2
}

void Stage::NotifyElementsChanged()
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Elements on stage have changed.");

	// Don't do anything unless there's a good chance this window still
	// exists in some sort of valid state.
	// TODO - Is IsShown a good way to tell whether a window is still good?
	if (IsShown())
	{
		// Update our element borders (if necessary) and fix our cursor.
		if (mIsDisplayingBorders)
			InvalidateScreen();
		UpdateCurrentElementAndCursor();
	}
}

void Stage::EnterElement(Element *inElement, wxPoint &inPosition)
{
	ASSERT(inElement->GetEventDispatcher());
	inElement->GetEventDispatcher()->DoEventMouseEnter(inPosition);
}

void Stage::LeaveElement(Element *inElement, wxPoint &inPosition)
{
	ASSERT(inElement->GetEventDispatcher());
	inElement->GetEventDispatcher()->DoEventMouseLeave(inPosition);
}

void Stage::UpdateCurrentElementAndCursor(wxPoint &inPosition)
{
	// Find which element we're in.
	wxPoint pos = ScreenToClient(::wxGetMousePosition());
	Element *obj = FindLightWeightElement(pos);

	// Change the cursor, if necessary.  I haven't refactored this
	// into EnterElement/LeaveElement yet because of how we handle
	// mIsDisplayingXy.  Feel free to improve.
	if (!mGrabbedElement && (obj == NULL || obj != mCurrentElement))
	{
		if (mIsDisplayingXy)
			SetCursor(*wxCROSS_CURSOR);
		else
		{
			if (obj)
				SetCursor(obj->GetCursor());
			else
				SetCursor(wxNullCursor);
		}
	}

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

void Stage::UpdateCurrentElementAndCursor()
{
	UpdateCurrentElementAndCursor(ScreenToClient(::wxGetMousePosition()));
}

void Stage::InterpreterSleep()
{
    // Put our interpreter to sleep.
	// TODO - Keep track of who we're sleeping for.
    ASSERT(TInterpreter::HaveInstance() &&
           !TInterpreter::GetInstance()->Paused());
    TInterpreter::GetInstance()->Pause();
}

void Stage::InterpreterWakeUp()
{
	// Wake up our Scheme interpreter.
	// We can't check TInterpreter::GetInstance()->Paused() because
	// the engine might have already woken the script up on its own.
	// TODO - Keep track of who we're sleeping for.
    ASSERT(TInterpreter::HaveInstance());
    TInterpreter::GetInstance()->WakeUp();
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

void Stage::OnIdle(wxIdleEvent &inEvent)
{
	if (mWaitElement && mWaitElement->HasReachedFrame(mWaitFrame))
		EndWait();

	// Send an idle event to the Scheme engine occasionally.
	if (ShouldSendEvents() &&
		::wxGetLocalTimeMillis() > mLastIdleEvent + IDLE_INTERVAL)
	{
		mLastIdleEvent = ::wxGetLocalTimeMillis();

		// We only pass the idle event to just the card, and not any
		// of the elements.  Idle event processing is handled differently
		// from most other events; we let the scripting language work
		// out the details.
		GetEventDispatcher()->DoEventIdle(inEvent);
	}
}

void Stage::OnMouseMove(wxMouseEvent &inEvent)
{
	// Do any mouse-moved processing for our Elements.
	UpdateCurrentElementAndCursor();
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
	wxLogTrace(TRACE_STAGE_DRAWING, "Painting stage.");

    // Set up our drawing context, and paint the screen.
    wxPaintDC screen_dc(this);
    PaintStage(screen_dc, GetUpdateRegion());
}

void Stage::PaintStage(wxDC &inDC, const wxRegion &inDirtyRegion)
{
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
		DrawTextBorder(inDC);

		ElementCollection::iterator i = mElements.begin();
		for (; i != mElements.end(); i++)
			if ((*i)->IsShown())
				DrawElementBorder(inDC, *i);
	}
}

// XXX - these should be refactored, but it's just two lines they have 
// in common. I'm not sure if it's worth it. Feel free to do so if you
// want.
void Stage::DrawElementBorder(wxDC &inDC, Element *inElement)
{
	inDC.SetPen(*wxRED_PEN);
	inDC.SetBrush(*wxTRANSPARENT_BRUSH);

	inElement->DrawElementBorder(inDC);
}

void Stage::DrawTextBorder(wxDC &inDC)
{
	if (mTextCtrl->IsShown())
	{
		inDC.SetPen(*wxRED_PEN);
		inDC.SetBrush(*wxTRANSPARENT_BRUSH);
		
		// Draw the border *outside* our rectangle.
		wxRect r = mTextCtrl->GetRect();
		r.Inflate(1);
		inDC.DrawRectangle(r.x, r.y, r.width, r.height);
	}
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

void Stage::OnTextEnter(wxCommandEvent &inEvent)
{
    // Get the text.
    wxString text = FinishModalTextInput();
    
    // Set up a drawing context.
    wxMemoryDC dc;
    dc.SelectObject(GetBackgroundPixmap());
    
    // Prepare to draw the text.
    dc.SetTextForeground(mTextCtrl->GetForegroundColour());
    dc.SetTextBackground(mTextCtrl->GetBackgroundColour());
    dc.SetFont(mTextCtrl->GetFont());

    // Draw the text.
    // PORTING - These offsets are unreliable and platform-specific.
    wxPoint pos = mTextCtrl->GetPosition();
    dc.DrawText(text, pos.x + 5, pos.y);
    InvalidateRect(mTextCtrl->GetRect());
}

void Stage::OnLeftDown(wxMouseEvent &inEvent)
{
	// Restore focus to the stage.
	SetFocus();

	// Dispatch the event.
	EventDispatcher *disp = FindEventDispatcher(inEvent.GetPosition());
	disp->DoEventLeftDown(inEvent, false);
}

void Stage::OnLeftDClick(wxMouseEvent &inEvent)
{
	EventDispatcher *disp = FindEventDispatcher(inEvent.GetPosition());
	disp->DoEventLeftDown(inEvent, true);	
}

void Stage::OnLeftUp(wxMouseEvent &inEvent)
{
	EventDispatcher *disp = FindEventDispatcher(inEvent.GetPosition());
	disp->DoEventLeftUp(inEvent);
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

		// Copy our string to the clipboard.  This code snippet comes from
		// the wxWindows manual.
		if (wxTheClipboard->Open())
		{
			wxTheClipboard->SetData(new wxTextDataObject(str));
			wxTheClipboard->Close();
			mFrame->SetStatusText(wxString("Copied: ") + str);
		}
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
    mRectsToRefresh.MergeRect(inRect);
    Refresh(FALSE, &inRect);
}

void Stage::SaveGraphics(const wxRect &inBounds)
{
	wxMemoryDC srcDC, dstDC;
	srcDC.SelectObject(GetBackgroundPixmap());
	dstDC.SelectObject(mSavePixmap);
	dstDC.Blit(inBounds.x, inBounds.y, inBounds.width, inBounds.height,
			   &srcDC, inBounds.x, inBounds.y);
}

void Stage::RestoreGraphics(const wxRect &inBounds)
{
	wxMemoryDC srcDC, dstDC;
	srcDC.SelectObject(mSavePixmap);
	dstDC.SelectObject(GetBackgroundPixmap());
	dstDC.Blit(inBounds.x, inBounds.y, inBounds.width, inBounds.height,
			   &srcDC, inBounds.x, inBounds.y);
	InvalidateRect(inBounds);
}

void Stage::Screenshot(const wxString &inFilename)
{
	wxImage image = GetCompositingPixmap().ConvertToImage();
	image.SaveFile(inFilename, wxBITMAP_TYPE_PNG);
}

void Stage::ModalTextInput(const wxRect &inBounds,
                           const int inTextSize,
                           const wxColour &inForeColor,
                           const wxColour &inBackColor)
{
    ASSERT(!mTextCtrl->IsShown());

    // Update our control to have the right properties and show it.
    mTextCtrl->SetValue("");
    wxFont font(inTextSize, wxROMAN, wxNORMAL, wxNORMAL);
    mTextCtrl->SetFont(font);
    mTextCtrl->SetForegroundColour(inForeColor);
    mTextCtrl->SetBackgroundColour(inBackColor);
    mTextCtrl->SetSize(inBounds);
    mTextCtrl->Show();
    mTextCtrl->SetFocus();
	NotifyElementsChanged();

	InterpreterSleep();
}

wxString Stage::FinishModalTextInput()
{
    ASSERT(mTextCtrl->IsShown());

	InterpreterWakeUp();

	// Store our result somewhere useful.
	gVariableManager.SetString("_modal_input_text", mTextCtrl->GetValue());

    // Hide our text control and get the text.
    mTextCtrl->Hide();
	NotifyElementsChanged();
    return mTextCtrl->GetValue();
}

bool Stage::Wait(const wxString &inElementName, MovieFrame inUntilFrame)
{
	ASSERT(mWaitElement == NULL);

	// Look for our element.
	ElementCollection::iterator i =
		FindElementByName(mElements, inElementName);

	// Make sure we can wait on this element.
	// TODO - Refactor this error-handling code to a standalone
	// routine so we don't have to keep on typing it.
	const char *name = (const char *) inElementName;
	if (i == mElements.end())
	{
		gDebugLog.Caution("wait: Element %s does not exist", name);
		return false;
	}
	MovieElement *movie = dynamic_cast<MovieElement*>(*i);
	if (movie == NULL)
	{
		gDebugLog.Caution("wait: Element %s is not a movie", name);
		return false;		
	}

	// Return immediately (if we're already past the wait point) or
	// go to sleep for a while.
	if (movie->HasReachedFrame(inUntilFrame))
		gDebugLog.Log("wait: Movie %s has already past frame %d",
					  name, inUntilFrame);
	else
	{
		mWaitElement = movie;
		mWaitFrame = inUntilFrame;
		InterpreterSleep();
	}
	return true;
}

void Stage::EndWait()
{
	gDebugLog.Log("wait: Waking up.");
	ASSERT(mWaitElement != NULL);
	mWaitElement = NULL;
	mWaitFrame = 0;
	InterpreterWakeUp();
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
			TransitionResources r(client_dc, before, GetCompositingPixmap(),
								  mOffscreenFadePixmap);
			mTransitionManager->RunTransition(inTransition, inMilliseconds, r);
		}
	}

	// Draw our offscreen buffer to the screen, and mark that portion of
	// the screen as updated.
    // XXX - This may overwrite any Widget objects on our stage.  We need
    // to use something like wxCLIP_CHILDREN if we can find it.
	{
		wxClientDC client_dc(this);
		PaintStage(client_dc, mRectsToRefresh);
	}
	ValidateStage();
}

void Stage::AddElement(Element *inElement)
{
	// Delete any existing Element with the same name.
	(void) DeleteElementByName(inElement->GetName());

	// Add the new Element to our list.
	mElements.push_back(inElement);
	NotifyElementsChanged();
}

Element *Stage::FindElement(const wxString &inElementName)
{
	ElementCollection::iterator i =
		FindElementByName(mElements, inElementName);
	if (i == mElements.end())
		return NULL;
	else
		return *i;
}

Element *Stage::FindLightWeightElement(const wxPoint &inPoint)
{
	// Look for the most-recently-added Element containing inPoint.
	Element *result = NULL;
	ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); i++)
		if ((*i)->IsLightWeight() && (*i)->IsPointInElement(inPoint))
			result = *i;
	return result;
}

EventDispatcher *Stage::FindEventDispatcher(const wxPoint &inPoint)
{
	// If a grab is in effect, return the element immediately.
	if (mGrabbedElement)
		return mGrabbedElement->GetEventDispatcher();

	// Otherwise, look things up normally.
	Element *elem = FindLightWeightElement(inPoint);
	if (elem && elem->GetEventDispatcher())
		return elem->GetEventDispatcher();
	else
		return GetEventDispatcher();
}

void Stage::DestroyElement(Element *inElement)
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
		mCurrentElement = NULL;
	if (inElement == mWaitElement)
		EndWait();

	// Destroy the object.
	// TODO - Implemented delayed destruction so element callbacks can
	// destroy the element they're attached to.
	delete inElement;
}

bool Stage::DeleteElementByName(const wxString &inName)
{
	bool found = false;
	ElementCollection::iterator i = FindElementByName(mElements, inName);
	if (i != mElements.end())
	{
		// Completely remove from the collection first, then destroy.
		Element *elem = *i;
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
	for (; i != mElements.end(); ++i)
		if (dynamic_cast<IMediaElement*>(*i))
			return true;
	return false;
}

void Stage::EndMediaElements()
{
	ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); ++i) {
		IMediaElement *elem = dynamic_cast<IMediaElement*>(*i);
		if (elem) {
			gDebugLog.Log("Manually ending media: %s",
						  (*i)->GetName().mb_str());
			elem->EndPlayback();
		}
	}
}

void Stage::MouseGrab(Element *inElement)
{
	ASSERT(inElement->IsLightWeight());
	ASSERT(inElement->GetEventDispatcher());
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

void Stage::MouseUngrab(Element *inElement)
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
		mCurrentElement = NULL;

	// Release our grab.
	mGrabbedElement = NULL;
	ReleaseMouse();
	UpdateCurrentElementAndCursor();
}

bool Stage::ShouldSendMouseEventsToElement(Element *inElement)
{
	return !mGrabbedElement || (inElement == mGrabbedElement);
}
