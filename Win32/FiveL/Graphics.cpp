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
    hPen = ::CreatePen(PS_SOLID, thickness, theColor);
    hPenOld = (HPEN) ::SelectObject(hdc,hPen);

    // tell gView what rectangle we drew in
	dirty_rect.Set(ay, ax, by, bx);
    
    // perform adjustments on horizontal and vertical lines
    // 1. make them one pixel longer to match what the Mac does
    // 2. for thicknesses > 1, Windows will center the line on the upper
    //		left point, which isn't what we want, we want it to really be
    //		the upper left so move the coordinates by half the thickness
    //
	
	if (thickness > 1)
    	offset = thickness / 2;
    	
    if (ax == bx)
    {
		//if (thickness == 1)
    	//	by++;			// make the line one pixel longer
    	//else
    	//	ay++;			// make it start one pixel down
		by++;
    	
		dirty_rect.OffsetRight(thickness);
		dirty_rect.OffsetBottom(2);

    	ax += offset;		// plus, don't center on upper left
    	bx += offset;
    }
    else if (ay == by)
    {
    	//if (thickness == 1)
    	//	bx++;			// make the line one pixel longer 
    	//else
    	//	ax++;			// make it start one pixel to the right 
		bx++;
 
		dirty_rect.OffsetBottom(thickness);
		dirty_rect.OffsetRight(2);
    	
    	ay += offset;		// plus, don't center on upper left
    	by += offset;
    }
    
    gView->DirtyRect(&dirty_rect);
 
	::MoveToEx(hdc, ax, ay, NULL);
	
	/* WIN16
    ::MoveTo(hdc, ax, ay);
	*/

    ::LineTo(hdc, bx, by);      
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
