// -*- Mode: C++; tab-width: 4; -*-
/* =================================================================================
	CTouchZone.cp	
   ================================================================================= */

#include "THeader.h"

#include <iostream>
#include <Palettes.h>
#include <Sound.h>
#include <LStream.h>
#include <UDrawingState.h>

#include "TLogger.h"

#include "CMac5LApp.h"
#include "CPlayerView.h"
#include "CMoviePlayer.h"
#include "CTouchZone.h"
#include "CPlayerInput.h"
#include "CPicture.h"

USING_NAMESPACE_FIVEL
/**************************************
		GLOBALS
***************************************/

// cbo - to take care of the "clicking too fast" problem
static bool gDoingTZone = false;

extern CursHandle	gHandCursor;
		
/*==================================================================================
	TOUCHZONE constructor
  ==================================================================================*/

CTouchZone::CTouchZone(
	TRect 			&inBounds, 	// Button rect
	TCallback 		*cmd, 		// Command callback
	CPicture		*inPict,	// Default picture
	TPoint 			&loc, 		// Pic offset??
	const CursorType cursor)	// cursor (= HAND_CURSOR)
{
	mNormalTouch = true;

	StartIdling();
	
	SetupZone(inBounds, cmd, inPict, loc, cursor);
	LButton::FinishCreate();
}


/*==================================================================================
	BUTTPCX constructor
  ==================================================================================*/

//	CTouchZone(Rect &r, TString &cmd, char *pict, Point &loc, char *text,
//				const char *header = nil, const TString &secCmd = nil);

CTouchZone::CTouchZone(
	TRect 			&inBounds, 
	TCallback		*cmd, 
	CPicture		*inPict,
	TPoint	 		&loc, 
	const char 		*text, 
	const CursorType cursor,		// = HAND_CURSOR
	const char 		*stylesheet)	// = NULL
	: mStyleSheet(stylesheet), mTextBounds(inBounds), mText(text)
{
	mNormalTouch = false;
	
	StartIdling();
	
	SetupZone(inBounds, cmd, inPict, loc, cursor);
	LButton::FinishCreate();
}

void CTouchZone::SetupZone(	TRect 			&inBounds, 	// Button rect
							TCallback		*cmd, 		// Command text
							CPicture		*inPict,	// Default picture
							TPoint 			&loc, 		// Pic offset??
							const CursorType cursor)	// cursor
{
	mBounds = inBounds;
	Rect	macBounds = inBounds.GetRect();
	//Point	macLoc = loc.GetPoint();
	
	// Set the frame size, the pane's superview, and position within the superview.
	ResizeFrameTo(macBounds.right - macBounds.left, macBounds.bottom - macBounds.top, false);
	PutInside(gPlayerView, false);
	PlaceInSuperFrameAt(macBounds.left, macBounds.top, false);

	// Skanky hack to set pane ID
	PP::LArray &paneList = gPlayerView->GetSubPanes();
	SetPaneID((paneList.GetCount()) + 2000);

	// Set private data members
    mCallback = cmd;
    mCursor = cursor;

	// Set picture location (if specified)
    if (inPict != nil)
    {
    	mPicture = inPict;
    	
    	if ((loc.X() != 0) or (loc.Y() != 0))
			mPictLoc = loc;
		else 
			mPictLoc = inBounds.TopLeft();
	}
	else
		mPicture = nil;
}

// ---------------------------------------------------------------------------------
//		¥ ~CTouchZone
// ---------------------------------------------------------------------------------

CTouchZone::~CTouchZone()
{
	StopIdling();
	
	if (mPicture != nil)
		mPicture->Purge();		// done with this picture

	delete mCallback;
}

/* ---------------------------------------------------------------------------------
 		¥ FinishCreateSelf
 		
 		Call this once the object is created. You can use it to take care of any
 		outstanding initialization issues.
   --------------------------------------------------------------------------------- */

void
CTouchZone::FinishCreateSelf()
{
	// If we have a style sheet, then we're a buttpcx and should draw some
	// text.
	if (mStyleSheet != "")
	{
		Rect 	frameRect;
		SInt32	fontHeight;
	
		CalcLocalFrameRect(frameRect);
	
		// Center text vertically (justification is set by header)
    	int32 dl = frameRect.bottom - frameRect.top;
    	fontHeight = gStyleSheetManager.GetLineHeight(mStyleSheet);

    	dl -= fontHeight;
    	dl /= 2;

		mTextBounds.Offset(TPoint(0, dl));
	
		// Get the offscreen gworld from the card view, and draw into it.
		CGWorld *theGWorld = gPlayerView->GetGWorld();
		GWorldPtr macGWorld = theGWorld->GetMacGWorld();

		theGWorld->BeginDrawing();		// Draw to offscreem GWorld

		// Non-buttpcx touchzones should only draw when pressed.
		if ((mPicture != nil) and (not mNormalTouch))
		{
			mPicture->Draw(mPictLoc, macGWorld, true);
		}
	
		theGWorld->EndDrawing();

		gStyleSheetManager.DoText(mStyleSheet, mTextBounds, mText, gPlayerView);
	}
	
	Enable();
}

void
CTouchZone::DrawSelf()
{
}

// ---------------------------------------------------------------------------
//		¥ HotSpotAction
// ---------------------------------------------------------------------------
//	Take action during mouse down tracking
//
//	Buttons toggle between two graphics, depending on whether the mouse is
//	inside or outside the button.

void
CTouchZone::HotSpotAction(
	SInt16		/* inHotSpot */,
	Boolean		inCurrInside,
	Boolean		inPrevInside)
{		
	if (not gPlayerView->ProcessingTZones())
		return;

	// Draw if cursor moved from IN to OUT or from OUT to IN.
	if (inCurrInside != inPrevInside)
	{	
		FocusDraw();

		if (mPicture != nil)
		{
			CGWorld *theGWorld = gPlayerView->GetGWorld();
			StCGWorldDrawingContext begin_drawing(theGWorld);
			GWorldPtr macGWorld = theGWorld->GetMacGWorld();
			
			// trying doing the same thing for both - this assumes that
			// the picture given for "normal" touch zones highlighting
			// has a non-highlight version
			if (!inCurrInside)
			{
				mPicture->Draw(mPictLoc, macGWorld, true);
			}
			else
			{
				CPicture *hilitePict = mPicture->GetHilitePicture();
				if (hilitePict != NULL)
					hilitePict->Draw(mPictLoc, macGWorld, true);
			}
		}
		
		// If we have a style sheet, then we're a buttpcx and should
		// draw some text.
		if (mStyleSheet != "")
		{
			// Decide whether or not to highlight our text.
			TString text = mText;
			if (inCurrInside)
				text = TString("^") + text + TString("^");
			
			// Draw our text using the specified style sheet.
			gStyleSheetManager.DoText(mStyleSheet, mTextBounds, text,
									  gPlayerView);
		}

		// Update our display.  Make sure that we're no longer
		// drawing to our GWorld when we do this!
		gPlayerView->Draw(nil);
	}
}


void
CTouchZone::HotSpotResult(
	SInt16	inHotSpot)
{
	if (not gPlayerView->ProcessingTZones())
	{
		//::SysBeep(30);
		return;
	}
	
	if (gDoingTZone)
		return;
		
	gDoingTZone = true;
	
	//if (clickSound != NULL)
	//	SndPlay(nil, (SndListResource **) clickSound, false);

	gDebugLog.Log("hit touchzone: <L T R B> %d %d %d %d, running callback",
				  mBounds.Left(), mBounds.Top(),
				  mBounds.Right(), mBounds.Bottom());

	// cbo - suspend event processing while we are executing this callback
	gPlayerView->ProcessEvents(false);
	
	mCallback->Run();
    
	// the callback might have changed something in the offscreen buffer
    gPlayerView->Draw(nil);			
   		
	if (not HaveInputUp())
		gPlayerView->ProcessEvents(true);
		
	gDoingTZone = false;
}


//
//	AdjustMouseSelf - CodeWarrior calls this when it thinks the mouse might need to be
//                    updated.  We let our built-in cursor manager handle the details.
//
void
CTouchZone::AdjustMouseSelf(Point inPortPt,
					        const EventRecord&  /* inMacEvent */,
					        RgnHandle           /* outMouseRgn */)
{
	Point	localPoint = inPortPt;	
	PortToLocalPoint(localPoint);
	gCursorManager.CheckCursor(localPoint);
}
