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

#include "THeader.h"

#include "CCursor.h"

#include "CPlayerView.h"
#include "CTouchZone.h"
#include "CMoviePlayer.h"

USING_NAMESPACE_FIVEL

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
CursorType CCursorManager::FindCursor(TString &inString)
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
	PP::LArray			&paneList = gPlayerView->GetSubPanes();
		
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
	PP::LArray			&paneList = gPlayerView->GetSubPanes();
	PP::LArrayIterator	iterator(paneList, PP::LArrayIterator::from_Start);
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
		Cursor arrow;
		::MacSetCursor(PP::UQDGlobals::GetArrow(&arrow));
	}
	
	if (prevCursor == NO_CURSOR)
		::ShowCursor();
}
								
	

/*
$Log$
Revision 1.6  2002/06/20 16:32:56  emk
Merged the 'FiveL_3_3_4_refactor_lang_1' branch back into the trunk.  This
branch contained the following enhancements:

  * Most of the communication between the interpreter and the
    engine now goes through the interfaces defined in
    TInterpreter.h and TPrimitive.h.  Among other things, this
    refactoring makes will make it easier to (1) change the interpreter
    from 5L to Scheme and (2) add portable primitives that work
    the same on both platforms.
  * A new system for handling callbacks.

I also slipped in the following, unrelated enhancements:

  * MacOS X fixes.  Classic Mac5L once again runs under OS X, and
    there is a new, not-yet-ready-for-prime-time Carbonized build.
  * Bug fixes from the "Fix for 3.4" list.

Revision 1.5.6.1  2002/06/15 01:06:54  emk
3.3.4.7 - Carbonization of Mac build, support for running non-Carbonized build
in MacOS X's OS 9 emulator, and basic support for 5L.prefs on the Mac.  The
Carbon build isn't yet ready for prime time--see BugHunt for details--but it
is good enough to use for engine development.

* Language changes

  - CHECKDISC is gone; use CHECKVOL instead.
  - EJECT is disabled in the Carbon build, because Carbon has no way to
    identify CD drives reliably.  EJECT still works in the regular build.
  - Gamma fades are ignored in the Carbon build.
  - KEYBINDs must now be accessed with the Command key only--not Option.

* Things to test

Please be hugely brutal to 5L; this is a big update.

  - 8-bit systems, palettes, ORIGIN, EJECT on the non-Carbon build.

* Internal changes

  - TException class (and all subclasses) now take a __FILE__ and __LINE__
    parameter.  This is ugly, but it allows me to debug 5L exceptions even
    without a working debugger (under the OS 9 emulator, for example).
  - FileSystem::Path::(DoesExist|IsRegularFile|IsDirectory) now rely on
    native MacOS File Manager calls instead of the broken MSL stat()
    function (which fails in the OS 9 emulator).
  - The ImlUnit test harness flushes its output more often.
  - Many data structure accessors (and such functions as c2pstr) have been
    replaced by their Carbon equivalents.
  - We now use PowerPlant accessors to get at the QuickDraw globals.
  - We now use PowerPlant calls in place of ValidRect and InvalRect.
  - Some very nasty code which set the palettes of our offscreen GWorlds
    has been removed (offscreen GWorlds have CLUTs, not palettes!).
    The various drawing commands now use gPaletteManager to map indexes
    to RGBColor values, and RGBForeColor to set the color--no more calls
    to ::PmForeColor on offscreen GWorlds, thank you!
  - The CMenuUtil code (which used low-memory system globals to hide
    and show the menu bar) has been removed entirely and replaced by
    calls to HideMenuBar and ShowMenuBar (which are present in 8.5 and
    Carbon).  This is much simpler, nicer, more portable and safer.
  - A bunch of code which had been disabled with #ifdefs has been
    removed entirely.  This mostly related to palettes and an obsolete
    version of the fade code which used GWorlds.
  - Code which used ROM-based KCHR resources to map option keys back to
    their unmodified key caps has been removed.  This means KEYBINDs
    can only be accessed using the Command key.
  - We assume Carbon systems always support the HFS file system (duh).
  - We use PowerPlant glue to access either StandardFile or Navigation
    Services, under OS 8/9 and Carbon, respectively.
  - Some old subroutines in CModuleManager appeared to have been
    snarfed from More Files, an old Mac utility library.  These have
    been moved into MoreFiles.{h,cpp}.

* Known Carbon Problems

Fades, ejecting CD-ROMs and playing QuickTime movies are all broken in
the Carbon build.  Douglas has found a problem with ORIGIN.  It looks
like we should continue to ship the OS 9 build for use with MacOS X,
at least for next few months.

Revision 1.5  2002/05/15 11:05:26  emk
3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
is available, and several subsystems have been refactored.  For more
detailed descriptions, see the CVS branch.

The merged Mac code hasn't been built yet; I'll take care of that next.

Revision 1.4.4.1  2002/05/15 09:09:47  emk
Added code to unfading the screen before displaying errors (this is needed to play nicely with the Mac gamma fader).

Migrated the Mac (buttpcx ...) to the new anti-aliased typography library.

The TBTree destructor is still a broken nightmare, especially on FatalError's forced shutdowns.  Expect FiveL to do something childish immediately after fatal errors and assertion failures.

Revision 1.4  2002/03/04 15:41:36  hamon
Changed calls to KString, KRect etc to TString, TRect, etc to reflect new names of merged common code.
Added namespace support for compiler. New files that use TString or TArray need to specify the FiveL namespace to differentiate between Code Warrior's template string and array classes and FiveL's common string and array classes.
Changes by Elizabeth and Eric, okayed by Eric.

Revision 1.3  2000/05/11 12:56:09  chuck
v 2.01 b1

Revision 1.2  2000/02/01 16:50:48  chuck
Fix cursors on overlapping touch zones.

Revision 1.1  2000/01/04 13:40:20  chuck
New cursors

*/