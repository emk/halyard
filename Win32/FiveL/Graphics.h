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
// Graphics.h : 
//

#if !defined (_Graphics_h_)
#define _Graphics_h_

#include "TRect.h"
#include "GraphicsTools.h"

/*-----------------------------------------------------------------

LIBRARY
    FiveL

-----------------------------------------------------------------*/

//  Drawing Routines

//////////
// Draw a line from point a to point b with the given color and thickness. 
// The background palette is used for the color palette.
//
// [in] x1 - the X-coordinate of the starting point
// [in] y1 - the Y-coordinate of the starting point
// [in] x2 - the X-coordinate of the ending point
// [in] y2 - the Y-coordinate of the ending point
// [in] color - color to use for drawing the line
// [in] thickness - line thickness
//
void    DrawLine(int x1,int y1, int x2, int y2, GraphicsTools::Color color,
		 int thickness = 1);

//////////
// Draw a rectangle with the given coordinates.  The background palette is 
// used for the color palette.
//
// [in] x1 - the X-coordinate of the upper-left point
// [in] y1 - the Y-coordinate of the upper-left point
// [in] x2 - the X-coordinate of the lower-right point
// [in] y2 - the Y-coordinate of the lower-right point
// [in] color - color to use for drawing the rectangle
// [in] filled - if true, flood fills the rectangle
//
void    DrawRect(int x1,int y1, int x2, int y2, GraphicsTools::Color color,
		 int filled = false);

//////////
// Draw a rectangle with the given coordinates.  The background palette is 
// used for the color palette.
//
// [in] inRect - structure for specifying a rectangular region
// [in] color - color to use for drawing the rectangle
// [in] filled - if true, flood fills the rectangle
//
void	DrawRect(TRect &inRect, GraphicsTools::Color inColor,
		 int inFilled = false);

//////////
// Draw a circle at the given coordinates.  The background palette is 
// used for the color palette.
//
// [in] x - the X-coordinate of the center point
// [in] y - the Y-coordinate of the center point
// [in] radius - radius of the circle
// [in] color - color to use for drawing the circle
//
void    DrawCircle(int x, int y, int radius, GraphicsTools::Color color);

//////////
// Produces the system defualt beep.
//
// [in] freq - FOR FUTURE USE (currently ignored)
// [in] duration - FOR FUTURE USE (currently ignored)
//
void    Beep(int freq = 1500, int duration = 80);

#endif // _Graphics_h_

/*
 $Log$
 Revision 1.3  2002/10/08 21:42:25  emk
 Palette removal, part 1:

   * All primitives which used to take palette indices now take RGB colors.
   * Old 5L: Added DEFPALETTE command for declaring palettes without
     BMP files.  This provides backwards compatibility for old code.

 I haven't removed the palette code yet, but plan to do so as soon as the
 migration is complete.

 Revision 1.2  2002/06/20 16:32:54  emk
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

 Revision 1.1.10.1  2002/06/05 20:42:38  emk
 3.3.4.2 - Broke Win5L dependencies on TIndex file by moving various pieces
 of code into TWin5LInterpreter.  Windows 5L now accesses the interpreter
 through a well-defined API.  Changes:

   * Removed many direct and indirect #includes of TIndex.h.
   * Added a TInterpreter method ReloadScript, which can be called by the
     higher-level ReDoScript command.
   * Checked in some files which should have been included in the 3.3.4.1
     checkin--these files contain the initial refactorings of Card and Macro
     callsites to go through the TInterpreter interface.

 Up next: Refactor various Do* methods out of Card and into a procedural
 database.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.3  2000/04/07 17:05:15  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
