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

#if !defined (_LTouchZone_h_)
#define _LTouchZone_h_

#include "TObject.h"
#include "TArray.h"
#include "TRect.h"
#include "TPoint.h"
#include "TString.h"
#include "TInterpreter.h"
#include "LPicture.h"
#include "LCursor.h"

/*-----------------------------------------------------------------

CLASS
    LTouchZone

	Represents a mouseable touch zone, which fires off a callback when
	the touch zone is activated.  Touch zones are rectangular
	and contain an LPicture and optionally text within the touch zone region.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LTouchZone : public TObject 
{
    public:
        //////////
		// Touchzone constructor.  Initialize the touch zone. 
		//
		// [in] r - rectangular region that is mouseable
		// [in] callback - callback to run when the touch zone is selected
		// We assume ownership of this object, and delete it when we're done.
		// [in] inCursor - CursorType for the cursor when over the touch zone
		// [in] pict - an LPicture to be displayed for the touch zone
		// [in] loc - top-left screen location where pict should be placed
		//
		LTouchZone(TRect &r, TCallback *callback, CursorType inCursor,
				   LPicture *pict, TPoint &loc);
        
		//////////
		// Buttpcx constructor.  Initialize the touch zone. 
		//
		// [in] r - rectangular region that is mouseable
		// [in] callback - callback to run when the touch zone is selected
		// We assume ownership of this object, and delete it when we're done.
		// [in] inCursor - CursorType for the cursor when over the touch zone
		// [in] pict - an LPicture to be displayed for the touch zone
		// [in] loc - top-left screen location where pict should be placed
		// [in] text - text to draw in the touch zone
		// [in] header - header to use to draw text
		//
		LTouchZone(TRect &r, TCallback *callback, CursorType inCursor,
				   LPicture *pict, TPoint &loc,
				   const char *text, TString &header);
        
	    //////////
		// Destructor.
		//
		virtual ~LTouchZone();

        //////////
		// Is the given point contained within the touch zone boundary?
		//
		// [in] where - the point
		// [out] return - true(!=0) if the point is contained within 
		//				  the rectangle, false(0) otherwise
		//
		int			Hit(TPoint &where) { return itsBounds.Contains(where); }
        
		//////////
		// Execute the touch zone's callback.
		// Highlight the associated picture if there is one, and same with
		// TEXT, if any.
		//
		// Note: Highlighted picture drawn directly to screen (not offscreen
		// buffer).
		//
		void		DoCallback();
        
		//////////
		// Get the associated picture.
		//
		// [out] return - LPicture associated with the touchzone
		//
		LPicture	*GetPict() { return itsPict; }
        
		//////////
		// Get the location of the associated picture.
		//
		// [in/out] pt - place the location in this TPoint
		//
		void		GetLoc(TPoint *pt) { *pt = itsPictLoc; }
        
		//////////
		// Get the first character of the touch zone text.
		//
		// [out] return - the first character of the touch zone text
		//
		char		FirstChar();
        
		//////////
		// Is there text diplayed with this touch zone?
		//
		// [out] return - true if it has text, false otherwise
		//
		bool		HasText() { return (not itsText.IsEmpty()); }
		
		//////////
		// Get the CursorType to be displayed when mousing over this touchzone 
		//
		// [out] return - the CursorType displayed on mouse over
		//
		CursorType	GetCursor(void) { return (cursor); }
		
		//////////
		// Get the touch zone boundary.
		//
		// [out] return - a TRect specifying the mousable touch zone
		//
		TRect		&GetBounds() { return (itsBounds); }

	protected:
		//////////
		// Touchzone region.
		//
		TRect		itsBounds;
        
		//////////
		// Picture displayed in the touchzone area.
		//
		LPicture    *itsPict;
        
		//////////
		// Callback associated with the touchzone.
		//
		TCallback	*itsCallback;
        
		//////////
		// Top-Left corner of picture location.
		//
		TPoint		itsPictLoc;
        
		//////////
		// Text drawn in the touchzone.
		//
		TString		itsText;
        
		//////////
		// Header used for the text.
		//
		TString     headerText;
        
		//////////
		// Cursor displayed when over the touchzone.
		//
		CursorType	cursor;
};

/*-----------------------------------------------------------------

CLASS
    LTouchZoneManager

	Manages a set of LTouchZone objects.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LTouchZoneManager : public TArray 
{
    public:
		//////////
		// Constructor.
		//
		LTouchZoneManager();
        
		//////////
		// Destructor.
		//
		virtual		~LTouchZoneManager() {}
		
		//////////
		// Remove all the LTouchZone objects in the array.
		//
		void		RemoveAll(void);
        
		//////////
		// Delete all the LTouchZone objects in the array.
		//
		void		Clear(void);
        
		//////////
		// Remove the touch zone at the given top left boundary.
		//
		// [in] left - left side of boundary
		// [in] top - top of boundary
		//
		void		Clear(int left, int top);

        //////////
		// Return the touch zone where the given point hit (if any).
		// Search the array from the end so that later defined zones are
		// on top.
		//
		// [in] where - the TPoint to check against
		// [out] return - an LTouchZone that contains this point, or NULL
		//				  if none were found	
		//
		LTouchZone	*GetTouchZone(TPoint &where);
        
		//////////
		// Return a LTouchZone if:<br>
		//		1. the enter key was pressed and there is only one zone<br>
		//		2. the first key of the text was pressed<br>
		//
		// [in] WPARAM - the key pressed
		// [out] return - LTouchZone invoked by keypress or NULL if none found
		//
		LTouchZone	*GetTouchZone(WPARAM wParam);
};

#endif // _LTouchZone_h_

/*
 $Log$
 Revision 1.1.10.1  2002/06/06 05:47:30  emk
 3.3.4.1 - Began refactoring the Win5L interpreter to live behind an
 abstract interface.

   * Strictly limited the files which include Card.h and Macro.h.
   * Added TWin5LInterpreter class.
   * Made as much code as possible use the TInterpreter interface.
   * Fixed a few miscellaneous build warnings.

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
