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

#include "THeader.h"

#include "TString.h"

BEGIN_NAMESPACE_FIVEL

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
		CursorType	FindCursor(TString &inString);
		
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

END_NAMESPACE_FIVEL
		
		
/*
$Log$
Revision 1.4  2002/05/15 11:05:26  emk
3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
is available, and several subsystems have been refactored.  For more
detailed descriptions, see the CVS branch.

The merged Mac code hasn't been built yet; I'll take care of that next.

Revision 1.3.4.1  2002/05/15 09:09:47  emk
Added code to unfading the screen before displaying errors (this is needed to play nicely with the Mac gamma fader).

Migrated the Mac (buttpcx ...) to the new anti-aliased typography library.

The TBTree destructor is still a broken nightmare, especially on FatalError's forced shutdowns.  Expect FiveL to do something childish immediately after fatal errors and assertion failures.

Revision 1.3  2002/03/04 15:41:37  hamon
Changed calls to KString, KRect etc to TString, TRect, etc to reflect new names of merged common code.
Added namespace support for compiler. New files that use TString or TArray need to specify the FiveL namespace to differentiate between Code Warrior's template string and array classes and FiveL's common string and array classes.
Changes by Elizabeth and Eric, okayed by Eric.

Revision 1.2  2000/05/11 12:56:09  chuck
v 2.01 b1

Revision 1.1  2000/01/04 13:40:20  chuck
New cursors

*/