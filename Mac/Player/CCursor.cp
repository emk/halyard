/////////////////////////////////////////////////////////////////////////////////////////
//
//		(c) Copyright 1999, Trustees of Dartmouth College. All rights reserved
//			Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//			$Date$
//			$Revision$
//
//
/////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////
//
//	CCursor.cp
//
//

#include "KHeader.h"

#include "CCursor.h"

#include "CPlayerView.h"
#include "CTouchZone.h"
#include "CMoviePlayer.h"

extern bool gFadedIn;

//static const int kLeftResID = 128;
//static const int kRightResID = 129;
//static const int kDownResID = 130;
//static const int kUpResID = 131;
static const int kHandResID = 132;
//static const int kBackupResID = 133;

static const int kRightHandResID = 134;
static const int kLeftHandResID = 135;
//static const int kUpHandResID = 136;
static const int kTurnRightResID = 137;
static const int kTurnLeftResID = 138;

static const char *kNoCursorStr = "none";
static const char *kArrowCursorStr = "arrow";
static const char *kWatchCursorStr = "watch";
static const char *kCrossCursorStr = "cross";
static const char *kHandCursorStr = "hand";
static const char *kRightCursorStr = "right";
static const char *kLeftCursorStr = "left";
static const char *kTurnRightCursorStr = "turnright";
static const char *kTurnLeftCursorStr = "turnleft";
//static const char *kUpCursorStr = "up";
//static const char *kDownCursorStr = "down";
//static const char *kBackupCursorStr = "backup";

CCursorManager::CCursorManager()
{	
	mCursorPos.h = 0;
	mCursorPos.v = 0;
	
	mCurrentCursor = NO_CURSOR;
	mDefaultCursor = ARROW_CURSOR;
	mHandCursor = NULL;
	mLeftCursor = NULL;
	mRightCursor = NULL;
	mTurnLeftCursor = NULL;
	mTurnRightCursor = NULL;
	mCursorShowing = false;
	mForceShow = false;
}

CCursorManager::~CCursorManager()
{
}

void CCursorManager::Init(void)
{
	::InitCursor();
	mCurrentCursor = NO_CURSOR;
	mDefaultCursor = ARROW_CURSOR;
	mCursorShowing = false;
	mForceShow = false;
	
	// read in all of our cursors
	mWatchCursor = ::MacGetCursor(watchCursor);
	mCrossCursor = ::MacGetCursor(crossCursor);
	
	// use the hand cursors
	mHandCursor = ::MacGetCursor(kHandResID);
	mLeftCursor = ::MacGetCursor(kLeftHandResID);
	mRightCursor = ::MacGetCursor(kRightHandResID);
	mTurnLeftCursor = ::MacGetCursor(kTurnLeftResID);
	mTurnRightCursor = ::MacGetCursor(kTurnRightResID);
}

void CCursorManager::HideCursor(void)
{
	if (mCursorShowing)
	{
		::HideCursor();
		mCursorShowing = false;
	}
}

void CCursorManager::ShowCursor(void)
{
	if (not mCursorShowing)
	{
		::ShowCursor();
		mCursorShowing = true;
	}
}

//
//	FindCursor - Given a string, return the cursor constant.
//
CursorType CCursorManager::FindCursor(KString &inString)
{
	CursorType	cursor;
	
	if  (inString.Equal(kWatchCursorStr, false))
		cursor = WATCH_CURSOR;
	else if (inString.Equal(kCrossCursorStr, false))
		cursor = CROSS_CURSOR;
	else if (inString.Equal(kHandCursorStr, false))
		cursor = HAND_CURSOR;
	else if (inString.Equal(kLeftCursorStr, false))
		cursor = LEFT_CURSOR;
	else if (inString.Equal(kRightCursorStr, false))
		cursor = RIGHT_CURSOR;
	else if (inString.Equal(kTurnLeftCursorStr, false))
		cursor = TURN_LEFT_CURSOR;
	else if (inString.Equal(kTurnRightCursorStr, false))
		cursor = TURN_RIGHT_CURSOR;
	else if (inString.Equal(kNoCursorStr, false))
		cursor = NO_CURSOR;
	else
		cursor = UNKNOWN_CURSOR;
		
	return (cursor);
}

//
//	CheckCursor - 
//
void CCursorManager::CheckCursor(Point &inPoint)
{
	mCursorPos = inPoint;
	
	//prdebug("Set cursor to: x <%d>, y <%d>", inPoint.h, inPoint.v);
		
	CheckCursor();
}

//
//	CheckCursor - See if the cursor is over a touch zone. If so, display the cursor 
//		for that zone (or the hand by default).
//
void CCursorManager::CheckCursor(void)
{
	LArray			&paneList = gPlayerView->GetSubPanes();
		
#ifdef DEBUG	
	if (not gFadedIn)
	{
		HideCursor();
		return;
	}

	if (gMovieManager.MoviePlaying())	// default cursor for movie playback
	{
		ChangeCursor(mDefaultCursor);
		ShowCursor();
		return;
	}
	
	CheckZones();
	ShowCursor();
#else
	if ((gMovieManager.MoviePlaying()) 	// no cursor during movie playback
	or (not gFadedIn))					// no cursor action when faded
	{
		// shouldn't be showing the mouse
		HideCursor();
		return;
	}

	if ((not mForceShow)				// not forcing the cursor visible	
	and (paneList.GetCount() == 0))		// no tzones, no cursor
	{
		HideCursor();
		return;
	}
	
	// should be showing the mouse
	CheckZones();
	ShowCursor();		
#endif
}	

//
//	CheckZones - Check the touch zones. Go through the list and remember the 
//		last zone that contains the cursor. We want touch zones defined later
//		ta take precedence.
//
void CCursorManager::CheckZones(void)
{
	CursorType		theCursor = NO_CURSOR;
	LArray			&paneList = gPlayerView->GetSubPanes();
	LArrayIterator	iterator(paneList, LArrayIterator::from_Start);
	CTouchZone		*theButt;
	bool			found = false;
	
	while (iterator.Next(&theButt))	
	{
		Rect	frameRect;
		
		theButt->CalcLocalFrameRect(frameRect);
		
		if (::PtInRect(mCursorPos, &frameRect))
		{
			// ask the touch zone for its cursor
			theCursor = theButt->GetCursor();
			found = true;
		}
	}
	
	if (found)
		ChangeCursor(theCursor, true);
	else
		ChangeCursor(mDefaultCursor);
}
	

//
//	ChangeCursor - Change the cursor to the given type.
//
void CCursorManager::ChangeCursor(CursorType inCursor, bool inTZone /* = false */)
{
	CursorType	prevCursor = mCurrentCursor;
	bool		SetArrow = false;

	if (not inTZone)
		mDefaultCursor = inCursor;
		
	if (mCurrentCursor == inCursor)
		return;
			
	switch (inCursor)
	{
		case NO_CURSOR:
			mCurrentCursor = NO_CURSOR;
			HideCursor();
			
			// cbo_fix 
			//prdebug("ChangeCursor: NO_CURSOR");
			
			break;
		case ARROW_CURSOR:
			SetArrow = true;
			
			// cbo_fix -
			//prdebug("ChangeCursor: ARROW_CURSOR");
			
			break;
		case WATCH_CURSOR:
			if (mWatchCursor != NULL)
			{
				mCurrentCursor = WATCH_CURSOR;
				::MacSetCursor(*mWatchCursor);
				
				// cbo_fix
				//prdebug("ChangeCursor: WATCH_CURSOR");
				
			}
			else
				SetArrow = true;
			break;
		case CROSS_CURSOR:
			if (mCrossCursor != NULL)
			{
				mCurrentCursor = CROSS_CURSOR;
				::MacSetCursor(*mCrossCursor);
				
				// cbo_fix
				//prdebug("ChangeCursor: CROSS_CURSOR");
			}
			else
				SetArrow = true;
			break;
		case HAND_CURSOR:
			if (mHandCursor != NULL)
			{
				mCurrentCursor = CROSS_CURSOR;
				::MacSetCursor(*mHandCursor);
				
				// cbo_fix 
				//prdebug("ChangeCursor: HAND_CURSOR");
				
			}
			else
				SetArrow = true;
			break;
		case LEFT_CURSOR:
			if (mLeftCursor != NULL)
			{
				mCurrentCursor = LEFT_CURSOR;
				::MacSetCursor(*mLeftCursor);
				
				// cbo_fix
				//prdebug("ChangeCursor: LEFT_CURSOR");
				
			}
			else
				SetArrow = true;
			break;
		case RIGHT_CURSOR:
			if (mRightCursor != NULL)
			{
				mCurrentCursor = RIGHT_CURSOR;
				::MacSetCursor(*mRightCursor);
				
				// cbo_fix
				//prdebug("ChangeCursor: RIGHT_CURSOR");
			}
			else
				SetArrow = true;
			break;
		case TURN_LEFT_CURSOR:
			if (mTurnLeftCursor != NULL)
			{
				mCurrentCursor = TURN_LEFT_CURSOR;
				::MacSetCursor(*mTurnLeftCursor);
				
				// cbo_fix
				// prdebug("ChangeCursor: TURN_LEFT_CURSOR");
			}
			else
				SetArrow = true;
			break;
		case TURN_RIGHT_CURSOR:
			if (mTurnRightCursor != NULL)
			{
				mCurrentCursor = TURN_RIGHT_CURSOR;
				::MacSetCursor(*mTurnRightCursor);
				
				// cbo_fix 
				// prdebug("ChangeCursor: TURN_RIGHT_CURSOR");
			}
			else
				SetArrow = true;
			break;
		default:
			SetArrow = true;
			break;
	}

	if (SetArrow)
	{
		mCurrentCursor = ARROW_CURSOR;
		::MacSetCursor(&UQDGlobals::GetQDGlobals()->arrow);
	}
	
	if (prevCursor == NO_CURSOR)
		::ShowCursor();
}
								
	

/*
$Log$
Revision 1.3  2000/05/11 12:56:09  chuck
v 2.01 b1

Revision 1.2  2000/02/01 16:50:48  chuck
Fix cursors on overlapping touch zones.

Revision 1.1  2000/01/04 13:40:20  chuck
New cursors

*/