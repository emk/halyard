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
// LCursor.h : 
//

#if !defined (_LCursor_h_)
#define _LCursor_h_

#include "TCommon.h"
#include "TRect.h"
#include "TPoint.h"
#include "TString.h"

//////////
// Enumeration of cursor types.
//
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
	LEFT_TURN_CURSOR,
	RIGHT_TURN_CURSOR
} CursorType;

/*-----------------------------------------------------------------

CLASS
    LCursorManager

	Manages the mouse cursor.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LCursorManager : public TObject
{
public:
	//////////
	// Constructor.
	//
	LCursorManager();

	//////////
	// Destructor.
	//
	~LCursorManager();

	//////////
	// Initialize all of our cursors.
	//
	// [in] inInstance - handle to application instance
	//
	void	Init(HINSTANCE inInstance);

	// DEPRECATED
	//
	//void	HideCursor(void);
	
	// DEPRECATED
	//
	//void	ShowCursor(void);

	//////////
	// Turn on/off forced showing of the cursor.
	//
	// [in] inForce - true, turns on forced show; false, turns off forced show
	//
	void	ForceShow(bool inForce) { mForceShow = inForce; }
	
	//////////
	// Set the cursor position and CheckCursor().
	//
	// [in] cursorPos - new cursor position
	//
	void	CheckCursor(TPoint &cursorPos);
	
	//////////
	// Check the state of the cursor and change it if necessary.
	//
	void	CheckCursor(void);
	
	//////////
	// Change to a different cursor.
	//
	// [in] inCursor - type of cursor to switch to
	// [in_optional] inTZone - Is the cursor in an LTouchZone? (default false)
	//
	void	ChangeCursor(CursorType inCursor, bool inTZone = false);
	
	//////////
	// Force a cursor redraw.
	//
	void	RedrawCursor(void);
	
	//////////
	// Find cursor type by name.
	//
	// [in] inCursor - name of a cursor
	// [out] return - type of the cursor
	//
	CursorType	FindCursor(TString &inCursor);

private:
	//////////
	// Current cursor.
	//
	CursorType	mCurrentCursor;
	
	//////////
	// Default cursor.
	//
	CursorType	mDefaultCursor;
	
	//////////
	// Show the cursor?
	//
	bool		mShowCursor;
	
	//////////
	// Force showing of cursor?
	//
	bool		mForceShow;

	//////////
	// Current cursor position.
	//
	TPoint		mCursorPos;
	
	//////////
	// Default Win32 arrow cursor.
	//
	HCURSOR		mArrowCursor;
	
	//////////
	// Hand cursor.
	//
	HCURSOR		mHandCursor;
	
	//////////
	// Hourglass cursor.
	//
	HCURSOR		mWatchCursor;
	
	//////////
	// Cross cursor.
	//
	HCURSOR		mCrossCursor;
	
	//////////
	// Left pointing hand cursor.
	//
	HCURSOR		mLeftCursor;
	
	//////////
	// Right pointing hand cursor.
	//
	HCURSOR		mRightCursor;
	
	//////////
	// Left turn cursor.
	//
	HCURSOR		mLeftTurnCursor;
	
	//////////
	// Right turn cursor.
	//
	HCURSOR		mRightTurnCursor;
};

#endif // _LCursor_h_

/*
 $Log$
 Revision 1.1.14.1  2002/10/11 18:03:30  emk
 3.4.3 - 11 Oct 2002 - emk

 Douglas--I don't have the scripts required to test this properly, so
 you'll have to arrange for both the smoke testing (i.e., does it work
 at all?) and the regular testing (i,e., does everything work right?).
 If there are problems, I'll build a 3.4.4 on Monday.

   * Removed code to clip cursor into box.  (Backported from 3.5.)
     This *should* fix a bug which caused the cursor to be locked at
     0,0 after startup on some machines.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.7  2000/04/07 17:05:16  chuck
 v 2.01 build 1

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
