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
#include "CText.h"
#include "CCard.h"
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
	TString 		&cmd, 		// Command text
	CPicture		*inPict,	// Default picture
	TPoint 			&loc, 		// Pic offset??
	const CursorType cursor,	// cursor (= HAND_CURSOR)
	const TString	&secCmd)	// optional second command (= NULL)
{
	mNormalTouch = true;

	StartIdling();
	
	SetupZone(inBounds, cmd, inPict, loc, secCmd, cursor);
	LButton::FinishCreate();
}


/*==================================================================================
	BUTTPCX constructor
  ==================================================================================*/

//	CTouchZone(Rect &r, TString &cmd, char *pict, Point &loc, char *text,
//				const char *header = nil, const TString &secCmd = nil);

CTouchZone::CTouchZone(
	TRect 			&inBounds, 
	TString 		&cmd, 
	CPicture		*inPict,
	TPoint	 		&loc, 
	const char 		*text, 
	const CursorType cursor,		// = HAND_CURSOR
	const char 		*header, 		// = NULL
	const TString 	&secCmd)		// = NULL
	: CText(header, inBounds, text)
{
	mNormalTouch = false;
	
	StartIdling();
	
	SetupZone(inBounds, cmd, inPict, loc, secCmd, cursor);
	LButton::FinishCreate();
}

void CTouchZone::SetupZone(	TRect 			&inBounds, 	// Button rect
							TString			&cmd, 		// Command text
							CPicture		*inPict,	// Default picture
//							char			*pict, 		// Default pic name
							TPoint 			&loc, 		// Pic offset??
							const TString	&secCmd,	// optional second command
							const CursorType cursor)	// cursor
{
	Rect	macBounds = inBounds.GetRect();
	//Point	macLoc = loc.GetPoint();
	
	// Set the frame size, the pane's superview, and position within the superview.
	ResizeFrameTo(macBounds.right - macBounds.left, macBounds.bottom - macBounds.top, false);
	PutInside(gPlayerView, false);
	PlaceInSuperFrameAt(macBounds.left, macBounds.top, false);

#ifdef DEBUG
	gDebugLog.Log("SetupZone: Left <%d>, Top <%d>, Right <%d>, Bottom <%d>",
		macBounds.left, macBounds.top, macBounds.right, macBounds.bottom);
#endif

	// Skanky hack to set pane ID
	PP::LArray &paneList = gPlayerView->GetSubPanes();
	SetPaneID((paneList.GetCount()) + 2000);

	// Set private data members
    mCommand = cmd;
    mSecondCommand = secCmd;
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
}

/* ---------------------------------------------------------------------------------
 		¥ FinishCreateSelf
 		
 		Call this once the object is created. You can use it to take care of any
 		outstanding initialization issues.
   --------------------------------------------------------------------------------- */

void
CTouchZone::FinishCreateSelf()
{
	Rect 	frameRect;
	SInt32	fontHeight;
	
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
		mPicture->Draw(mPictLoc, macGWorld, true);
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
	SInt16		/* inHotSpot */,
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
			if (not inCurrInside)
				mPicture->Draw(mPictLoc, (CGrafPort *) gPlayerView->GetMacPort(), true);
			else
			{
				CPicture *hilitePict = NULL;
				
				hilitePict = mPicture->GetHilitePicture();
				if (hilitePict != NULL)
					hilitePict->Draw(mPictLoc, (CGrafPort *) gPlayerView->GetMacPort(), true);
			}
		}
			
		DrawSimpleText(inCurrInside);
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
									// Undo Button hilighting
	HotSpotAction(inHotSpot, false, true);
	gPlayerView->Draw(nil);			// want to see the highlighted button
	
	//if (clickSound != NULL)
	//	SndPlay(nil, (SndListResource **) clickSound, false);

#ifdef DEBUG
	if (not mSecondCommand.IsEmpty())
		gDebugLog.Log("hit touchzone: commands <%s> then <%s>", mSecondCommand.GetString(), mCommand.GetString());
	else
		gDebugLog.Log("hit touchzone: command <%s>", mCommand.GetString());
#endif

	// cbo - suspend event processing while we are executing these commands
	gPlayerView->ProcessEvents(false);
	
    if (not mSecondCommand.IsEmpty())
    	gCardManager.DoOneCommand(mSecondCommand);
    
    gCardManager.DoOneCommand(mCommand);
    
    gPlayerView->Draw(nil);			// the command might have changed something in the offscreen buffer
   		
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
