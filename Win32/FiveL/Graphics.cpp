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
// Graphics.cpp : 
//

#include "stdafx.h"

#include "TCommon.h"
#include "LUtil.h"

#include "View.h"
#include "Graphics.h"
#include "Globals.h"


/***********************************************************************
 * Function: DrawLine
 *
 *  Parameter a  (startpt.)
 *  Parameter b  (endpt.)
 *  Parameter color
 *  Parameter thickness
 * Return:
 *
 * Comments:
 * Draw a line from point a to point b with the given
 *  color and thickness. Note:  background palette is used!
 ***********************************************************************/
void DrawLine(int ax, int ay, int bx, int by, int color, int thickness)
{
	COLORREF	theColor;
    HPEN    	hPen, hPenOld;
    HDC    		hdc; 
    TRect		dirty_rect;
    int			offset = 0;
    
	hdc = gView->GetDC();
    theColor = gView->GetColor(color);

    // tell gView what rectangle we drew in
	dirty_rect.Set(ay, ax, by, bx);
        	
	// We special-case the drawing of horizontal and vertical lines
	// because we want them to precisely match the Macintosh (we use them
	// in some fairly complicated line drawings, and it looks bad
	// if the lines get drawn in strange places).  We make two adjustments:
	//   1. We make all our line longer (by the thickness of the line)
	//      to match the Mac's behavior.
	//   2. We decompose each line into single-thickness lines.  This
	//      serves two purposes:
	//      a. It keeps Windows from centering the line on the upper-left
	//         co-ordinate, and instead uses that co-ordinate as the
	//         upper-left co-ordinate of a multi-pixel-wide line.
	//      b. It fixes bug #822, where certain Win98 and WinME systems
	//         started drawing wide lines (thickness > 1) one pixel too soon.
    if (ax == bx || ay == by)
    {
		hPen = ::CreatePen(PS_SOLID, 1, theColor);
		hPenOld = (HPEN) ::SelectObject(hdc,hPen);

		if (ax == bx)
		{
			by += thickness; // make the line one pixel longer
    	
			dirty_rect.OffsetRight(thickness);
			dirty_rect.OffsetBottom(2);

			for (int i = 0; i < thickness; i++)
			{
				::MoveToEx(hdc, ax + i, ay, NULL);
				::LineTo(hdc, bx + i, by);
			}
		}
		else if (ay == by)
		{
			bx += thickness; // make the line one pixel longer
 
			dirty_rect.OffsetBottom(thickness);
			dirty_rect.OffsetRight(2);
    	
			for (int i = 0; i < thickness; i++)
			{
				::MoveToEx(hdc, ax, ay + i, NULL);
				::LineTo(hdc, bx, by + i);
			}
		}
	}
    else // Diagonal line
	{
		// We probably fail to set up our dirty_rect in a perfectly
		// accurate fashion here.  This code isn't used much.
		hPen = ::CreatePen(PS_SOLID, thickness, theColor);
		hPenOld = (HPEN) ::SelectObject(hdc,hPen);
		::MoveToEx(hdc, ax, ay, NULL);
		::LineTo(hdc, bx, by);      
	}
	
    gView->DirtyRect(&dirty_rect);
    ::SelectObject(hdc,hPenOld);    	
    ::DeleteObject(hPen);
}

void DrawRect(TRect &inRect, int inColor, int inFilled /* = false */)
{
	DrawRect(inRect.Left(), inRect.Top(), inRect.Right(), inRect.Bottom(),
		inColor, inFilled);
}

/***********************************************************************
 * Function: DrawRect
 *
 *  Parameter r
 *  Parameter color
 *  Parameter filled
 * Return:
 *
 * Comments:
 *    Draw the rectangle r, color "color" and fill it if filled==true
 *  Same background palette trick. 
 ***********************************************************************/
void DrawRect(int x1,int y1, int x2, int y2, int color, int filled)
{
	COLORREF	theColor;
    HBRUSH  	hBrushTrans;
    RECT    	rect;
    HDC     	hdc;
    
	hdc = gView->GetDC();

    rect.top = y1;  
	rect.bottom = y2;
    rect.left = x1;  
	rect.right = x2;

    theColor = gView->GetColor(color);
    
    hBrushTrans = ::CreateSolidBrush(theColor);  
    if (!hBrushTrans)
    { 
    	gLog.Log("Error: Can't create solid rectanglebrush!");
    	return;
    }

    if (filled) 	
        ::FillRect(hdc, &rect, hBrushTrans);
    else
        ::FrameRect(hdc, &rect, hBrushTrans);
        
    gView->DirtyRect(&rect);
                                         
    ::DeleteObject(hBrushTrans);
}
                                               
/***********************************************************************
 * Function: DrawCircle
 *
 *  Parameter x
 *  Parameter y
 *  Parameter r
 *  Parameter color
 *  Parameter filled
 * Return:
 *
 * Comments:
 *  Draw a circle. Again, background palette is used.
 ***********************************************************************/
void DrawCircle(int x, int y, int radius, int color)  
{
	COLORREF	theColor;
    HBRUSH  	hBrushTrans;
    HRGN    	rect;
    HDC     	hdc;
	TRect		dirtyRect;
    
    hdc = gView->GetDC();
    theColor = gView->GetColor(color);
    rect = ::CreateEllipticRgn(x, y, x+2*radius, y+2*radius);
	
	dirtyRect.Set(y, x, y+2*radius, x+2*radius);
    gView->DirtyRect(&dirtyRect);
    
    hBrushTrans = ::CreateSolidBrush(theColor);
    ::FillRgn(hdc, rect, hBrushTrans);
    ::DeleteObject(hBrushTrans);
    ::DeleteObject(rect);   
}

/***********************************************************************
 * Function: Beep
 *
 *  Parameter freq
 *  Parameter duration
 * Return:
 *
 * Comments:
 *  Make noise. Default settings are 1500Hz, 80 milliseconds.
 ***********************************************************************/
void Beep(int freq, int duration)
{
    ::MessageBeep(-1);
}
    
/*
 $Log$
 Revision 1.1.2.2  2002/07/03 11:44:39  emk
 3.2.0.6 - All known, fixable 3.2.0.x Windows bugs should be fixed.  Please
 test this engine carefully, especially line drawing (including diagonal)
 and (touch ...) callbacks.

     Bug #958 workaround: Since VP 3.2.1.3 doesn't support GDI
     under the QT 6 Public Preview, 5L no longer forces QuickTime
     to use GDI.  See the bug report for details.  5L scripts
     can tell whether QuickTime is using DirectDraw (the most
     common alternative to GDI) by checking the variable
     _QuickTimeHasDDObject.

     Bug #980 revisted: Our fix to touchzone handling yesterday broke
     support for the second touchzone command.  This has been fixed.

     Bug #986 fixed: QuickTime no longer performs gamma correction
     when importing images, so we no longer see nasty color mismatches.

     Bug #822 fixed: When drawing lines with thickness > 1, we now
     decompose the lines into single-pixel lines to work around a bug
     in certain Win98 and WinME systems which caused lines to begin
     one pixel too soon.

 Revision 1.1.2.1  2002/03/13 15:06:56  emk
 Merged changed from 3.1.1 -> 3.2.1 into the 3.2.0.1 codebase,
 because we want these in the stable engine.  Highlights:

   1) FiveL.prefs file support.
   2) Removal of -D command line flag.

 Revision 1.2  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/04/07 17:05:15  chuck
 v 2.01 build 1

 Revision 1.3  2000/03/01 15:46:55  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
