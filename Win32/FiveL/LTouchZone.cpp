// -*- Mode: C++; tab-width: 4; -*-
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
// LTouchZone.cpp : 
//

#include "stdafx.h"

#include "LTouchZone.h"
#include "Globals.h"
#include "TStyleSheet.h"

#define BUTTONXSHIFT 2
#define BUTTONYSHIFT 3
#define ACTIVATED 8
#define NOT_ACTIV 7
#define BUTT_TXT_MAX    256

// Pause in milliseconds after button is highlighted
#define HILITE_PAUSE 200

//
//  Initialize the touch zone. If no location point is given, use
//  the picture's.
//
// used for touchzone
LTouchZone::LTouchZone(TRect &r, TCallback *callback, CursorType inCursor, 
					   LPicture *pict, TPoint &loc) : TObject()
{
    itsBounds = r;
    itsCallback = callback;
    itsPict = pict;
 	cursor = inCursor;

    if ((loc.X() == 0) and (loc.Y() == 0) and (pict != NULL))
		itsPictLoc = pict->GetOrigin();
    else
        itsPictLoc = loc;
}

// used for buttpcx
LTouchZone::LTouchZone(TRect &r, TCallback *callback, CursorType inCursor, 
					   LPicture *pict, TPoint &loc, const char *text, 
					   TString &header) : TObject()
{
    itsBounds = r;
    itsCallback = callback;
    itsPict = pict;
    itsText = text;
    headerText = header;
	cursor = inCursor;

    if ((loc.X() == 0) and (loc.Y() == 0) and (pict != NULL))
		itsPictLoc = pict->GetOrigin();
    else
        itsPictLoc = loc;
}

LTouchZone::~LTouchZone()
{
	delete itsCallback;
}    

char LTouchZone::FirstChar()
{
	if (not itsText.IsEmpty())
		return (itsText(0));
	return (0);
}

/***********************************************************************
 * Function: LTouchZone::DoCallback
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *  Execute the touch zone's callback.
 *  Hilite the associated picture if there is one, and same with TEXT, if any.
 *  Hilite picture always matted and always drawn directly to
 *  the screen.
 ***********************************************************************/
void LTouchZone::DoCallback()
{
    TString     header;
    TString     temp;
    TRect        bounds1;
    LPicture     *itsPictHilite;
    int         dl, fontHeight;

    bounds1.Set(itsBounds);

    if (itsPict != NULL)  
    {
        if (not itsText.IsEmpty())  
        {
	        temp = "^";
	        temp += itsText;              //Color Change.
	        temp += "^";
	
	        itsPictHilite = itsPict->GetHilitePicture();  //Hilited button...
	        itsPictHilite->Draw(itsPictLoc, true);
	
	        dl = bounds1.Bottom() - bounds1.Top();      //...and text...
	        fontHeight = gStyleSheetManager.GetLineHeight(headerText);
	        dl -= fontHeight;
	        dl /= 2;
			bounds1.OffsetTop(dl);
			bounds1.OffsetBottom(dl);
			gStyleSheetManager.DoText(headerText, bounds1, temp, gView);
	        
	        gView->Draw();		// blast the hilited stuff to the screen
	
			// brief pause needed to see hilited button (esp on fast machines)
			::Sleep(HILITE_PAUSE);

	        itsPict->Draw(itsPictLoc, true);
	        temp = itsText;
			gStyleSheetManager.DoText(headerText, bounds1, temp, gView);
        }
        else 
        	itsPict->Hilite(itsPictLoc, true); 
    }

	itsCallback->Run();	// run the callback
    gView->Draw();		// the callback executed might have changed something
}

//
//	LTouchZoneManager methods.
//

//
//	LTouchZoneManager - Construct the touch zone list.
//
LTouchZoneManager::LTouchZoneManager() : TArray()
{
}

//
//	RemoveAll
//
void LTouchZoneManager::RemoveAll(void)
{
	Clear();
}

//
//	Clear - Clear all touchzones.
//
void LTouchZoneManager::Clear(void)
{
	DeleteAll();
}

//
//	Clear - Clear the touch zone at left and top (if any). Only clear
//		the first one you come to.
//
void LTouchZoneManager::Clear(int left, int top)
{ 
	LTouchZone	*theZone;
    int32 		i;
    bool		done = false;
    
    i = 0;
    while ((not done) and (i < NumItems()))
    {
    	theZone = (LTouchZone *) Item(i);
    	
    	if (theZone != NULL)
    	{
			if ((theZone->GetBounds().Left() == left) and
				(theZone->GetBounds().Top() == top))
     		{
    			DeleteIndex(i);
    			done = true;
    		}
    	}
    	
    	i++;
    }
}

//
//	GetTouchZone - Return the touch zone where the given point hit (if any).
//		Search the array from the end so that later defined zones are
//		on top.
//
LTouchZone *LTouchZoneManager::GetTouchZone(TPoint &where)
{
	LTouchZone	*theZone;    
	int32		i;
    

 	for (i = NumItems() - 1; i >= 0; i--)
    {
    	theZone = (LTouchZone *) Item(i);
        
        if (theZone->Hit(where))
            return (theZone);
    }

    return (NULL);
}

//
//	GetTouchZone - Return a LTouchZone if:
//		1. the enter key was pressed and there is only one zone
//		2. the first key of the text was pressed		
//
LTouchZone *LTouchZoneManager::GetTouchZone(WPARAM wParam)
{
	int32		i;
	LTouchZone	*theZone;
	
	if ((NumItems() == 1) and (wParam == 13))
		return ((LTouchZone *) Item(0));
	else
	{
		for (i = 0; i < NumItems(); i++)
		{
			theZone = (LTouchZone *) Item(i);
			
			if ((theZone->HasText()) 
			and (tolower((char) wParam) == tolower(theZone->FirstChar())))
			{
				return (theZone);		
            }
		}
	}
	
	return (NULL);
}

/*
 $Log$
 Revision 1.5  2002/06/20 16:32:55  emk
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

 Revision 1.4.6.1  2002/06/06 05:47:30  emk
 3.3.4.1 - Began refactoring the Win5L interpreter to live behind an
 abstract interface.

   * Strictly limited the files which include Card.h and Macro.h.
   * Added TWin5LInterpreter class.
   * Made as much code as possible use the TInterpreter interface.
   * Fixed a few miscellaneous build warnings.

 Revision 1.4  2002/05/15 11:05:33  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.3.2.1  2002/05/15 09:23:56  emk
 3.3.2.8 - Last typography branch checkin before merge.

 * Fixed (wait ...) bug which caused all (wait ...) commands to wait
 until the end of the movie.

 * (buttpcx ...) now uses anti-aliased text.

 * Miscellaneous other tweaks and fixes--just getting things into shape
 for the merge.

 Revision 1.3  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.2  2002/01/23 20:39:20  tvw
 A group of changes to support a new stable build.

 (1) Only a single instance of the FiveL executable may run.

 (2) New command-line option "-D" used to lookup the installation directory in the system registry.
     Note: Underscores will be parsed as spaces(" ").
     Ex: FiveL -D HIV_Prevention_Counseling

 (3) Slow down the flash on buttpcx so it can be seen on
     fast machines.  A 200 mS pause was added.

 (4) Several bugfixes to prevent possible crashes when error
     conditions occur.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  1999/10/22 20:29:09  chuck
 New cursor management.

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
