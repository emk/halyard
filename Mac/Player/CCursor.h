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
//	CCursor.h - Class for managing cursors.
//
//

#pragma once

#include "KHeader.h"

#include "KString.h"

typedef enum
{
	UNKNOWN_CURSOR,
	NO_CURSOR,
	ARROW_CURSOR,
	WATCH_CURSOR,
	CROSS_CURSOR,
	HAND_CURSOR,
	LEFT_CURSOR,
	RIGHT_CURSOR,
	TURN_RIGHT_CURSOR,
	TURN_LEFT_CURSOR
} CursorType;

class CCursorManager
{
	public:
		CCursorManager();
		~CCursorManager();

		void		Init(void);
				
		void		HideCursor(void);
		void		ShowCursor(void);
		void		ForceShow(bool inForce)	{ mForceShow = inForce; }
		void		CheckCursor(void);
		void		CheckCursor(Point &inPoint);
		void		ChangeCursor(CursorType	inCursor, bool inTZone = false);
		void		SetCursorPos(Point &inPoint) { mCursorPos = inPoint; }
		CursorType	FindCursor(KString &inString);
		
	protected:
		void		CheckZones(void);
		
		CursorType	mCurrentCursor;
		CursorType	mDefaultCursor;
		Point		mCursorPos;
		bool		mCursorShowing;
		bool		mForceShow;

		CursHandle	mWatchCursor;
		CursHandle	mCrossCursor;
		CursHandle	mHandCursor;
		CursHandle	mLeftCursor;
		CursHandle	mRightCursor;
		CursHandle	mTurnLeftCursor;
		CursHandle	mTurnRightCursor;
};

extern CCursorManager gCursorManager;
		
		
/*
$Log$
Revision 1.2  2000/05/11 12:56:09  chuck
v 2.01 b1

Revision 1.1  2000/01/04 13:40:20  chuck
New cursors

*/