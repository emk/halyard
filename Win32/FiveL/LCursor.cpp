//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// LCursor.cpp : 
//

#include "stdafx.h"
#include "LCursor.h"
#include "Globals.h"
#include "View.h"
#include "resource.h"

const char *kArrowCursorStr = "arrow";
const char *kWatchCursorStr = "watch";
const char *kCrossCursorStr = "cross";
const char *kHandCursorStr = "hand";
const char *kLeftCursorStr = "left";
const char *kRightCursorStr = "right";
const char *kLeftTurnCursorStr = "turnleft";
const char *kRightTurnCursorStr = "turnright";

LCursorManager::LCursorManager()
{
	mHandCursor = NULL;
	mArrowCursor = NULL;
	mWatchCursor = NULL;
	mCrossCursor = NULL;
	mLeftCursor = NULL;
	mRightCursor = NULL;
	mLeftTurnCursor = NULL;
	mRightTurnCursor = NULL;
	mDefaultCursor = ARROW_CURSOR;
	cursorRect = NULL;
}

LCursorManager::~LCursorManager()
{
	if (mHandCursor != NULL)
		::DestroyCursor(mHandCursor);
	if (mArrowCursor != NULL)
		::DestroyCursor(mArrowCursor);
	if (mWatchCursor != NULL)
		::DestroyCursor(mWatchCursor);
	if (mCrossCursor != NULL)
		::DestroyCursor(mCrossCursor);
	if (mLeftCursor != NULL)
		::DestroyCursor(mLeftCursor);
	if (mRightCursor != NULL)
		::DestroyCursor(mRightCursor);
	if (mLeftTurnCursor != NULL)
		::DestroyCursor(mLeftTurnCursor);
	if (mRightTurnCursor != NULL)
		::DestroyCursor(mRightTurnCursor);

	::ClipCursor(NULL);
}

void LCursorManager::Init(HINSTANCE inInstance)
{
	// get our cursors and initialze to nothing
	mArrowCursor = ::LoadCursor(NULL, IDC_ARROW);
	mWatchCursor = ::LoadCursor(NULL, IDC_WAIT);
	mCrossCursor = ::LoadCursor(NULL, IDC_CROSS);
	mHandCursor = ::LoadCursor(inInstance, MAKEINTRESOURCE(IDC_HAND_CURSOR));
	mLeftCursor = ::LoadCursor(inInstance, MAKEINTRESOURCE(IDC_LEFT_CURSOR));
	mRightCursor = ::LoadCursor(inInstance, MAKEINTRESOURCE(IDC_RIGHT_CURSOR));
	mLeftTurnCursor = ::LoadCursor(inInstance, MAKEINTRESOURCE(IDC_TURN_LEFT_CURSOR));
	mRightTurnCursor = ::LoadCursor(inInstance, MAKEINTRESOURCE(IDC_TURN_RIGHT_CURSOR));

	mDefaultCursor = ARROW_CURSOR;
}

void LCursorManager::ClipCursor(TRect *inClipRect)
{
	RECT theRect;

	if (inClipRect != NULL)
	{
		cursorRect = new TRect(*inClipRect);
		theRect = inClipRect->GetRECT();
		::ClipCursor(&theRect);
	}
	else
		::ClipCursor(NULL);
}

// Removes cursor constraints
void LCursorManager::UnClipCursor()
{
	RECT theRect;

	if (cursorRect == NULL) 
	{
		::GetClipCursor(&theRect);
		cursorRect = new TRect();
		cursorRect->Set(theRect);
		
	}
	::ClipCursor(NULL);	

}

// Restore cursor clipping rect after a call to UnClipCursor
void LCursorManager::ReClipCursor()
{
	RECT theRect;

	theRect = cursorRect->GetRECT();
	::ClipCursor(&theRect);	
}

//
//	ChangeCursor - Set the cursor.
//
void LCursorManager::ChangeCursor(CursorType inCursor, bool inTZone /* = false */)
{
	bool	SetArrow = false;

	if ((not inTZone) and (inCursor != NO_CURSOR))
		mDefaultCursor = inCursor;

	if (mCurrentCursor == inCursor)
		return;

	switch (inCursor)
	{
		case NO_CURSOR:
			mCurrentCursor = NO_CURSOR;
			::SetCursor(NULL);
			break;
		case HAND_CURSOR:
			if (mHandCursor != NULL)
			{
				mCurrentCursor = HAND_CURSOR;
				::SetCursor(mHandCursor);
			}
			else
				SetArrow = true;
			break;
		case WATCH_CURSOR:
			if (mWatchCursor != NULL)
			{
				mCurrentCursor = WATCH_CURSOR;
				::SetCursor(mWatchCursor);
			}
			else
				SetArrow = true;
			break;
		case CROSS_CURSOR:
			if (mCrossCursor != NULL)
			{
				mCurrentCursor = CROSS_CURSOR;
				::SetCursor(mCrossCursor);
			}
			else
				SetArrow = true;
			break;
		case LEFT_CURSOR:
			if (mLeftCursor != NULL)
			{
				mCurrentCursor = CROSS_CURSOR;
				::SetCursor(mLeftCursor);
			}
			else
				SetArrow = true;
			break;
		case RIGHT_CURSOR:
			if (mRightCursor != NULL)
			{
				mCurrentCursor = RIGHT_CURSOR;
				::SetCursor(mRightCursor);
			}
			else
				SetArrow = true;
			break;
		case LEFT_TURN_CURSOR:
			if (mLeftTurnCursor != NULL)
			{
				mCurrentCursor = LEFT_TURN_CURSOR;
				::SetCursor(mLeftTurnCursor);
			}
			else
				SetArrow = true;
			break;
		case RIGHT_TURN_CURSOR:
			if (mRightTurnCursor != NULL)
			{
				mCurrentCursor = RIGHT_TURN_CURSOR;
				::SetCursor(mRightTurnCursor);
			}
			else
				SetArrow = true;
			break;
		case ARROW_CURSOR:
		default:
			SetArrow = true;
			break;
	}

	if (SetArrow)
	{
		mCurrentCursor = ARROW_CURSOR;
		::SetCursor(mArrowCursor);
	}
}

//
//	RedrawCursor - See if we can force a cursor redraw.
//
void LCursorManager::RedrawCursor(void)
{
	ChangeCursor(mCurrentCursor);
}

void LCursorManager::CheckCursor(TPoint &inCursorPos)
{
	mCursorPos = inCursorPos;

	CheckCursor();
}

//
//	FindCursor - 
//
CursorType LCursorManager::FindCursor(TString &inString)
{
	CursorType	cursor = ARROW_CURSOR;

	if (inString.Equal(kArrowCursorStr, false))
		cursor = ARROW_CURSOR;
	else if (inString.Equal(kWatchCursorStr, false))
		cursor = WATCH_CURSOR;
	else if (inString.Equal(kCrossCursorStr, false))
		cursor = CROSS_CURSOR;
	else if (inString.Equal(kHandCursorStr, false))
		cursor = HAND_CURSOR;
	else if (inString.Equal(kLeftCursorStr, false))
		cursor = LEFT_CURSOR;
	else if (inString.Equal(kRightCursorStr, false))
		cursor = RIGHT_CURSOR;
	else if (inString.Equal(kLeftTurnCursorStr, false))
		cursor = LEFT_TURN_CURSOR;
	else if (inString.Equal(kRightTurnCursorStr, false))
		cursor = RIGHT_TURN_CURSOR;
	else
		cursor = UNKNOWN_CURSOR;

	return (cursor);
}

//
//	CheckCursor - Check the state of the cursor. 
//
void LCursorManager::CheckCursor(void)
{ 
	LTouchZone	*theZone;
	int32		numZones;
	CursorType	theCursor = NO_CURSOR;
	bool		inTZone = false;
	
	numZones = gTouchZoneManager.NumItems();

	if ((not gView->Faded()) and (not gVideoManager.Playing()))
	{
		if (numZones > 0)
		{
			theZone = gTouchZoneManager.GetTouchZone(mCursorPos);
			
			if (theZone != NULL)
			{
				theCursor = theZone->GetCursor();
				inTZone = true;
			}
			else
				theCursor = mDefaultCursor;  	
		}
		else if (mForceShow)
			theCursor = mDefaultCursor;
	}

#ifdef DEBUG
	if (gVideoManager.Playing())
		theCursor = mDefaultCursor;
	else if (numZones <= 0)
		theCursor = mDefaultCursor;
#endif

	ChangeCursor(theCursor, inTZone);
}

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.8  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.7  2000/01/04 18:52:51  chuck
 no message

 Revision 1.6  2000/01/04 13:32:56  chuck
 New cursors

 Revision 1.5  1999/11/02 17:16:37  chuck
 2.00 Build 8

 Revision 1.4  1999/10/27 19:42:40  chuck
 Better cursor management

 Revision 1.3  1999/10/22 20:29:09  chuck
 New cursor management.

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
