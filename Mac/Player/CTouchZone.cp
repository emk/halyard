/* =================================================================================
	CTouchZone.cp	
   ================================================================================= */

#include "debug.h"

#include <iostream>
#include <Palettes.h>
#include <Sound.h>
#include <LStream.h>
#include <UDrawingState.h>

#include "Mac5L.h"
#include "CMac5LApp.h"
#include "CPlayerView.h"
#include "CMoviePlayer.h"
#include "CTouchZone.h"
#include "CPlayerInput.h"
#include "CText.h"
#include "CCard.h"
#include "CPicture.h"


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
	Rect 			&inBounds, 	// Button rect
	CString 		&cmd, 		// Command text
	CPicture		*inPict,	// Default picture
	Point 			&loc, 		// Pic offset??
	const CursorType cursor,	// cursor (= HAND_CURSOR)
	const CString	&secCmd)	// optional second command (= NULL)
{
	mNormalTouch = true;

	StartIdling();
	
	SetupZone(inBounds, cmd, inPict, loc, secCmd, cursor);
	LButton::FinishCreate();
}


/*==================================================================================
	BUTTPCX constructor
  ==================================================================================*/

//	CTouchZone(Rect &r, CString &cmd, char *pict, Point &loc, char *text,
//				const char *header = nil, const CString &secCmd = nil);

CTouchZone::CTouchZone(
	Rect 			&inBounds, 
	CString 		&cmd, 
	CPicture		*inPict,
	Point	 		&loc, 
	char 			*text, 
	const CursorType cursor,		// = HAND_CURSOR
	const char 		*header, 		// = NULL
	const CString 	&secCmd)		// = NULL
	: CText(header, inBounds, text)
{
	mNormalTouch = false;
	
	StartIdling();
	
	SetupZone(inBounds, cmd, inPict, loc, secCmd, cursor);
	LButton::FinishCreate();
}

void CTouchZone::SetupZone(	Rect 			&inBounds, 	// Button rect
							CString			&cmd, 		// Command text
							CPicture		*inPict,	// Default picture
//							char			*pict, 		// Default pic name
							Point 			&loc, 		// Pic offset??
							const CString	&secCmd,	// optional second command
							const CursorType cursor)	// cursor
{
	// Set the frame size, the pane's superview, and position within the superview.
	ResizeFrameTo(inBounds.right - inBounds.left, inBounds.bottom - inBounds.top, false);
	PutInside(gPlayerView, false);
	PlaceInSuperFrameAt(inBounds.left, inBounds.top, false);

	// Skanky hack to set pane ID
	LArray &paneList = gPlayerView->GetSubPanes();
	SetPaneID((paneList.GetCount()) + 2000);

	// Set private data members
    mCommand = cmd;
    mSecondCommand = secCmd;
    mCursor = cursor;

	// Set picture location (if specified)
    if (inPict != nil)
    {
    	mPicture = inPict;
    	//mPicture = GetPicture(pict);
    	
    	if ((loc.h != 0 || loc.v != 0))
			mPictLoc = loc;
		else 
			mPictLoc = topLeft(inBounds);
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
	
	// cbo_mem
	// cbo - only do this if the resource tree isn't going to manage memory
	// for us
	//if (mPicture != nil)
	//	mPicture->Purge();		// done with this picture
}

/* ---------------------------------------------------------------------------------
 		¥ FinishCreateSelf
 		
 		Call this once the object is created. You can use it to take care of any
 		outstanding initialization issues.
   --------------------------------------------------------------------------------- */

void
CTouchZone::FinishCreateSelf()
{
	Rect frameRect;
	Int32	fontHeight;
	
	CalcLocalFrameRect(frameRect);
	
	// Center text vertically (justification is set by header)
    int32 dl = frameRect.bottom - frameRect.top;
    fontHeight = GetLineHeight();

    dl -= fontHeight;
    dl /= 2;

	::OffsetRect(&mDrawRect, 0, dl);
	
	// Get the offscreen gworld from the card view, and draw into it.
	CGWorld *theGWorld = gPlayerView->GetGWorld();
	GWorldPtr macGWorld = theGWorld->GetMacGWorld();

	theGWorld->BeginDrawing();		// Draw to offscreem GWorld

	// Non-buttpcx touchzones should only draw when pressed.
	if ((mPicture != nil) and (not mNormalTouch))
	{
		mPicture->DrawPic(mPictLoc, BITMAP(macGWorld), true);
	}
		
	DrawSimpleText(false);
	
	theGWorld->EndDrawing();

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
	Int16		/* inHotSpot */,
	Boolean		inCurrInside,
	Boolean		inPrevInside)
{
		
	if (not gPlayerView->ProcessingTZones())
	{
		//::SysBeep(30);
		return;
	}
									// Draw if cursor moved from IN to OUT
									//   or from OUT to IN
	if (inCurrInside != inPrevInside)
	{	
		FocusDraw();

		if (mPicture != nil)
		{
			// trying doing the same thing for both - this assumes that the picture
			// given for "normal" touch zones highlighting has a non-highlight
			// version
			//
			//if (not mNormalTouch)
			//{
				if (not inCurrInside)
					mPicture->DrawPic(mPictLoc, BITMAP(gPlayerView->GetMacPort()), true);
				else
					mPicture->DrawHiPic(mPictLoc, BITMAP(gPlayerView->GetMacPort()), true);
			//}
			//else
			//	mPicture->DrawHiPic(mPictLoc, BITMAP(gPlayerView->GetMacPort()), true);
		}
			
		DrawSimpleText(inCurrInside);
	}
}


void
CTouchZone::HotSpotResult(
	Int16	inHotSpot)
{
	if (not gPlayerView->ProcessingTZones())
	{
		//::SysBeep(30);
		return;
	}
	
	if (gDoingTZone)
		return;
		
	gDoingTZone = true;
									// Undo Button hilighting
	HotSpotAction(inHotSpot, false, true);
	gPlayerView->Draw(nil);			// want to see the highlighted button
	
	//if (clickSound != NULL)
	//	SndPlay(nil, (SndListResource **) clickSound, false);

#ifdef DEBUG_5L
	if (not mSecondCommand.empty())
		prinfo("hit touchzone: commands <%s> then <%s>", mSecondCommand.GetString(), mCommand.GetString());
	else
		prinfo("hit touchzone: command <%s>", mCommand.GetString());
#endif

	// cbo - suspend event processing while we are executing these commands
	gPlayerView->ProcessEvents(false);
	
    if (not mSecondCommand.empty())
    	gCardManager.DoOneCommand(mSecondCommand);
    
    gCardManager.DoOneCommand(mCommand);
    
    gPlayerView->Draw(nil);			// the command might have changed something in the offscreen buffer
   		
	if (not HaveInputUp())
		gPlayerView->ProcessEvents(true);
		
	gDoingTZone = false;
}

//
//	MouseEnter - The mouse has entered our touch zone.
//
void
CTouchZone::MouseEnter(Point /* inPortPtr */, const EventRecord& /* inMacEvent */)
{
	gCursorManager.CheckCursor;
}

//
//	MouseLeave - The mouse just left our touch zone. 
//
void
CTouchZone::MouseLeave()
{
	gCursorManager.CheckCursor();
}

//
//	MouseWithin - If the mouse is within our touch zone, change the cursor to
//			the hand (if we have it).
//
void
CTouchZone::MouseWithin(Point /* inPortPt */, const EventRecord& /* inMacEvent */)
{
	gCursorManager.ChangeCursor(mCursor);
}
