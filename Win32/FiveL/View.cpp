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
                 
#include "stdafx.h"

#include "TCommon.h"
#include "LUtil.h"             

#include "Dib.h" 
#include "Globals.h"

#include "Palettes.h"

View::View()
{ 
	HDC		hDC;
	
	hDC = ::GetDC(NULL);
	m_bit_depth = ::GetDeviceCaps(hDC, BITSPIXEL);	

	if (m_bit_depth > 8)
		m_dib_bit_depth = 24;
	else
		m_dib_bit_depth = 8;

	// topdown should always be false
	m_topdown = false;

	// set the rest of the stuff
	m_new_pal = false;
}

View::~View()
{
	HBITMAP	hold;

	if (m_gworld != NULL)
	{
		DisposeGWorld(m_gworld); 
		m_gworld = NULL;
	}
	
	hold = (HBITMAP) ::SelectObject(m_dc, m_old_bmp);
	::DeleteObject(hold);	
	::DeleteDC(m_dc);

	hold = (HBITMAP) ::SelectObject(m_blippo_dc, m_old_blippo_bmp);
	::DeleteObject(hold);
	::DeleteDC(m_blippo_dc);
}

bool View::Init(void)
{
	HDC		hDC;
	DWORD	lastError;
	QDErr	qdErr;
	
	hDC = ::GetDC(hwndApp);

	// CreateDIBSection - match the display's pixel depth
	m_screen = NULL;
	m_blippo = NULL;
	m_screen_bits = NULL;
	m_blippo_bits = NULL;
	m_gworld = NULL;

    // set these fields with information about our bitmap
	m_bmp_info.biSize = sizeof(BITMAPINFOHEADER);
	m_bmp_info.biHeight = -1 * VSCREEN_HEIGHT;	// always a top down dib
	m_bmp_info.biWidth = VSCREEN_WIDTH;
	m_bmp_info.biPlanes = 1;
	m_bmp_info.biBitCount = m_dib_bit_depth; 
	m_bmp_info.biCompression = BI_RGB;
	m_bmp_info.biSizeImage = 0;
	m_bmp_info.biXPelsPerMeter = 0;
	m_bmp_info.biYPelsPerMeter = 0;
	m_bmp_info.biClrUsed = 0;
	m_bmp_info.biClrImportant = 0;
	
	// create the offscreen buffer
	m_screen = ::CreateDIBSection(NULL, (PBITMAPINFO) &m_bmp_info, DIB_RGB_COLORS, 
		(void **) &m_screen_bits, NULL, 0);

	if (m_screen == NULL)
	{
		lastError = ::GetLastError();

		::ReleaseDC(hwndApp, hDC);
		gLog.Error("Not enough memory to allocate off screen buffer. Error <%ld>. Quitting!",
			lastError);
		return (false);
	}

	// create the blippo buffer
	m_bmp_info.biSize = sizeof(BITMAPINFOHEADER);
	m_bmp_info.biHeight = -1 * VSCREEN_HEIGHT;	// always a top down dib
	m_bmp_info.biWidth = VSCREEN_WIDTH;
	m_bmp_info.biPlanes = 1;
	m_bmp_info.biBitCount = m_dib_bit_depth; 
	m_bmp_info.biCompression = BI_RGB;
	m_bmp_info.biSizeImage = 0;
	m_bmp_info.biXPelsPerMeter = 0;
	m_bmp_info.biYPelsPerMeter = 0;
	m_bmp_info.biClrUsed = 0;
	m_bmp_info.biClrImportant = 0;

	m_blippo = ::CreateDIBSection(NULL, (PBITMAPINFO) &m_bmp_info, DIB_RGB_COLORS,
		(void **) &m_blippo_bits, NULL, 0);

	if (m_blippo == NULL)
	{
		lastError = ::GetLastError();

		::ReleaseDC(hwndApp, hDC);
		gLog.Error("Not enough memory to allocate blippo buffer. Error <%ld>. Quitting!",
			lastError);
		return(false);
	}

	// select the offscreen buffer
	m_dc = ::CreateCompatibleDC(hDC);
	m_old_bmp = (HBITMAP) ::SelectObject(m_dc, m_screen);

	m_blippo_dc = ::CreateCompatibleDC(hDC);
	m_old_blippo_bmp = (HBITMAP) ::SelectObject(m_blippo_dc, m_blippo);

	// create a Mac GWorld for the QuickTime Image components

	qdErr = ::NewGWorldFromHBITMAP(&m_gworld, NULL, NULL, 0, m_screen, m_dc);
	if (qdErr != noErr)
	{
		gLog.Error("Could not create GWorld for QuickTime imaging, error <%ld>", qdErr);
		::ReleaseDC(hwndApp, hDC);
		return (false);
	}
	   	
	m_pal = NULL;			// no palette yet
	m_faded = false; 		// not faded
	m_locked = false; 		// not locked
	m_blippoed = false;		// and not blippoed
	m_dirty = false;		// and nothing to draw 
	m_num_rects = 0;		// no dirty rects

	::ReleaseDC(hwndApp, hDC);

	return (true);
}

//
//	GetDC - Return the DC for the offscreen world.
//
HDC View::GetDC(void)
{ 
	// for now, set m_dirty to true
	//m_dirty = true;
	
	return (m_dc);
}

void View::DirtyRect(TRect *inRect)
{
	RECT	dirtyRect;

	if (inRect != NULL)
	{
		dirtyRect = inRect->GetRECT();
		DirtyRect(&dirtyRect);
	}
	else
		DirtyRect(NULL, true);
}

//
//	DirtyRect - The given rectangle is dirty. Put it in the
//		list of dirty rectangles to ensure it gets drawn the
//		next time. If NULL is passed in, mark the whole screen
//		as dirty.
//
void View::DirtyRect(RECT *inRect, bool inWholeScreen /* = false */)
{
	RECT	testRect;
	int32	i;
	bool	doneRect;
	 
	if ((m_dirty) and (m_num_rects == 0))
	{
//		gDebugLog.Log("DirtyRect: already doing whole screen");
		return;
	}
		
	if ((inWholeScreen) or (inRect == NULL))
	{
		m_num_rects = 0;
//		gDebugLog.Log("DirtyRect: NULL -> whole screen");
	}
	else if (m_num_rects + 1 >= MAX_RECTS)
	{
		m_num_rects = 0;				// do the whole screen
//		gDebugLog.Log("DirtyRect: too many rects -> whole screen");
	}
	else
	{
		// see if the given rect intersects with a rect already in the list
		// if so, merge the two 
		doneRect = false;
		for (i = 0; (i < m_num_rects) and (not doneRect); i++)
		{
			if (::IntersectRect(&testRect, inRect, &(m_rects[i])))
			{ 
				::UnionRect(&(m_rects[i]), inRect, &(m_rects[i]));
				doneRect = true; 
//				gDebugLog.Log("DirtyRect: intersection, result: T <%d>, L <%d>, B <%d>. R <%d>",
//					m_rects[i].top, m_rects[i].left, m_rects[i].bottom, m_rects[i].right);
			}
		}
		
		if (not doneRect)
		{		
			m_rects[m_num_rects] = *inRect;	// save the rectangle
			m_num_rects++;
//			gDebugLog.Log("DirtyRect: T <%d>, L <%d>, B <%d>. R <%d>",
//				inRect->top, inRect->left, inRect->bottom, inRect->right);
		}
	} 
	
	m_dirty = true;
}

//
//	SetPalette - Set our palette. We reset the color table
//		of our offscreen world and remember the HPALETTE
//		to set it when we want to blast to the screen.
//
void View::SetPalette(LPalette *inPal)
{
	HPALETTE		hPal;
	CTabHandle		ctab;
	PALETTEENTRY 	logicalPal[256];
    
	if (inPal == NULL)
		return;
	
	m_pal = inPal;

	if (m_dib_bit_depth > 8)
		return;			// nothing else to do
	
	// set the color table for our offscreen world
	hPal = inPal->GetPalHandle();
	if (hPal == NULL)
	{
		gDebugLog.Log("View::SetPalette: no HPALLETE to set");
		return;
	}

	::GetPaletteEntries(hPal, 0, 256, logicalPal); 
	::SetDIBColorTable(m_dc, 0, 256, (const RGBQUAD *) logicalPal);

	ctab = inPal->GetCTabHandle();
	if (ctab == NULL)
	{
		gDebugLog.Log("View::SetPalette: no CTabHandle to set");
		return;
	}

	Rect	scrRect;
	scrRect = gScreenRect.GetRect();

	gDebugLog.Log("View::SetPalette: setting GWorld color table");
	::UpdateGWorld(&m_gworld, m_dib_bit_depth, &scrRect, ctab, NULL, clipPix);

	m_new_pal = true;
} 
		
void View::Draw(bool inForce)
{ 
	HDC			hDC;  
	HPALETTE	oldPal;

	if (m_bit_depth <= 8)
	{
		if (m_pal == NULL)
			return;
	}
	
	if ((m_locked) or (m_faded) or (gVideoManager.Playing()))
	{
		return;		// no drawing to screen when locked or faded or movie playing
	}
	
	if ((not inForce) and (not m_dirty)) 
	{
		return;
	}

	m_dirty = false;		// have drawn the screen
		
	hDC = ::GetDC(hwndApp);
	
	if ((m_bit_depth <= 8) and (m_pal != NULL))
	{
		HPALETTE	hPal;

		hPal = m_pal->GetPalHandle();

	// we are in  SYSPAL_NOSTATIC mode so unrealize our
		// palette before selecting it
		::UnrealizeObject(hPal);
		oldPal = ::SelectPalette(hDC, hPal, false);
		::RealizePalette(hDC);

		// use QuickTime to realize the palette
//		if (m_new_pal)
//		{
//			WindowPtr		wPtr;
//			PaletteHandle	hPalette;
//			CTabHandle		cTabHand;
//
//			cTabHand = m_pal->GetCTabHandle();
//
//			if (cTabHand != NULL)
//			{
//				wPtr = ::GetNativeWindowPort(hwndApp);
//				hPalette = ::NewPalette((**cTabHand).ctSize+1, cTabHand, pmTolerant, 0);
//
//				// Hook the palette to the window
//				::NSetPalette(wPtr, hPalette, pmAllUpdates);
//				::ActivatePalette(wPtr);
//			}
//
//			m_new_pal = false;
//		}
	}
		
	if ((m_num_rects == 0) or (inForce))
	{
		// blast the whole screen
		::BitBlt(hDC, 0, 0, VSCREEN_WIDTH, VSCREEN_HEIGHT, m_dc, 0, 0, SRCCOPY);
	}
	else
	{ 
		// traverse the list of rectangles		
		for (int i = 0; i < m_num_rects; i++)
		{		
			::BitBlt(hDC, m_rects[i].left, m_rects[i].top, 
				m_rects[i].right - m_rects[i].left, m_rects[i].bottom - m_rects[i].top,
				m_dc, m_rects[i].left, m_rects[i].top, SRCCOPY); 
		}
	}

	// Use QuickTime to blast the offscreen world to the screen
//	if (m_gworld != NULL)
//	{
//		Rect	r;
//		GWorldPtr	oldPort;
//		GDHandle	oldGD;
//
//		GetGWorld(&oldPort, &oldGD);
//		SetGWorld((GWorldPtr)GetNativeWindowPort(hwndApp), 0);
//
//		// set fore and back colors so StdBits doesn't mess up the colors
//		RGBColor color;
//		color.red = color.blue = color.green = 0;
//		RGBForeColor(&color);
//		color.red = color.blue = color.green = 0xFFFF;
//		RGBBackColor(&color);
//
//		if ((m_num_rects == 0) or (inForce))
//		{
//			r.left = r.top = 0;
//			r.right = VSCREEN_WIDTH;
//			r.bottom = VSCREEN_HEIGHT;
//			StdBits((BitMap*)*m_gworld->portPixMap, &r, &r, srcCopy, 0L);
//		}
//		else
//		{
//			for (int i = 0; i < m_num_rects; i++)
//			{
//				r.left = (short) m_rects[i].left;
//				r.top = (short) m_rects[i].top;
//				r.right = (short) m_rects[i].right;
//				r.bottom = (short) m_rects[i].bottom;
//				StdBits((BitMap*)*m_gworld->portPixMap, &r, &r, srcCopy, 0L);
//			}
//		}
//
//		SetGWorld(oldPort, oldGD);
//	}
	
	m_num_rects = 0;			// reset dirty rectangles

	::ReleaseDC(hwndApp, hDC); 
}

//
//	Blippo - Copy the offscreen world to the blippo world.
//		Copy the color table too??
//
void View::Blippo(void)
{
    // copy all the bits
	BlippoCopy(true);
}

//
//	UnBlippo - Restore the saved bitmap created by blippo
//		with an optional effect. The content that we want
//		to effect onto the screen is already in the blippo
//		buffer so we can just call DoEffect right away.
//
void View::UnBlippo(TString &inEffect, long inTime)
{
	Effect	theEffect;
	
	theEffect = StringToEffect(inEffect);
	DoEffect(theEffect, inTime);
}

//                        
//	Lock - Go to lock mode where we won't blast the offscreen
//		stuff to the screen.
//
void View::Lock(bool inClear)
{
	if (inClear)
		ClearScreen(0);			// make the buffer all black
		
    Draw();						// make sure the screen is up to date
    
    m_locked = true;			// set locked flag
    gVariableManager.SetLong("_locked", 1);
}

//
// Unlock - Unlock with an optional effect.
//
void View::Unlock(TString &inEffect, long inTime)
{
	Effect	theEffect;
	
	theEffect = StringToEffect(inEffect);
	
	m_locked = false;			// unset the locked flag
	gVariableManager.SetLong("_locked", 0);
	
	if ((theEffect == transNONE) or (inTime == 0))
		Draw();					// just blast to the screen
	else
	{
		Blippo();				// blippo first to make copy of what we are
								// going to effect onto the screen
		DoEffect(theEffect, inTime); 
	}
}

//
//	ClearScreen - Clear the screen to the given color.
//
void View::ClearScreen(int inColor)
{
	HBRUSH		theBrush;
	COLORREF	theColor;
	RECT		theRect;
	
	theColor = GetColor(inColor);
	
	theBrush = ::CreateSolidBrush(theColor);
	if (theBrush == NULL)
	{
		gDebugLog.Log("ClearScreen: couldn't create brush for <%d>", inColor);
		return;
	}
	
	m_dirty = true;
		
	theRect.top = 0;
	theRect.left = 0;
	theRect.right = VSCREEN_WIDTH;
	theRect.bottom = VSCREEN_HEIGHT;

	gDebugLog.Log("ClearScreen: to <%d>", inColor);
	
	::FillRect(m_dc, &theRect, theBrush);
	::DeleteObject(theBrush);		 
}

//
//	BlackScreen - Fill the screen with black (not the offscreen
//			buffer!). This is useful after fades and other times.
//
void View::BlackScreen(void)
{
	HDC		hDC;
	HGDIOBJ	oldBrush;

	gDebugLog.Log("View::BlackScreen");

	hDC = ::GetDC(hwndApp);
	oldBrush = ::SelectObject(hDC, ::GetStockObject(BLACK_BRUSH));
	::Rectangle(hDC, 0, 0, VSCREEN_WIDTH + 1, VSCREEN_HEIGHT + 1);
	::SelectObject(hDC, oldBrush);
	::ReleaseDC(hwndApp, hDC);
}

//
//	GetColor - Return a COLORREF with info about the given color in the
//		current palette.
//
COLORREF View::GetColor(int inColor)
{
	COLORREF		theRef = RGB(0, 0, 0);
	
	if (m_pal != NULL)
		theRef = m_pal->GetColor(inColor);

	return (theRef);
}

//
//	CopyDIB - Copy a DIB into the offscreen buffer. We always copy the entire DIB.
//   
void View::CopyDIB(Dib *inDib, POINT *inPt, BYTE inTransColor, bool inTrans)
{
	HDC		dibDc;
	RECT	dirtyRect;		// rectangle that we have dirtied
	long	dibH;			// height of dib
	long	dibW;			// width of dib 

	// sanity check on pointer parameters	
	if ((inDib == NULL) or (inPt == NULL))
		return;
		
	// sanity check on inPt
	if ((inPt->x > VSCREEN_WIDTH) or (inPt->y > VSCREEN_HEIGHT))
		return;

	// see if we need to adjust the height of the DIB
	dibH = inDib->Height();
	if ((dibH + inPt->y) > VSCREEN_HEIGHT)
		dibH = VSCREEN_HEIGHT - inPt->y;
		
	// do the same for the width of the DIB
	dibW = inDib->Width();
	if ((dibW + inPt->x) > VSCREEN_WIDTH)
		dibW = VSCREEN_WIDTH - inPt->x;
	
	//m_dirty = true;  
	dirtyRect.top = inPt->y;
	dirtyRect.left = inPt->x;
	dirtyRect.bottom = (int16) (inPt->y + dibH);
	dirtyRect.right = (int16) (inPt->x + dibW);
	if ((dirtyRect.top == 0) and 
		(dirtyRect.left == 0) and
		(dirtyRect.bottom > (VSCREEN_HEIGHT - 5)) and 
		(dirtyRect.right > (VSCREEN_WIDTH - 5)))
	{
		DirtyRect(&dirtyRect, true);				// dirty the whole screen
	}
	else
	{
		DirtyRect(&dirtyRect); 
	}

	dibDc = ::CreateCompatibleDC(m_dc);
	::SelectObject(dibDc, inDib->GetDib());
	
	if (not inTrans)
	{
		BitBlt(m_dc, inPt->x, inPt->y, dibW, dibH, dibDc, 0, 0, SRCCOPY);
	}
	else
	{
		COLORREF	transColor = 0x00FFFFFF;	// white by default
		COLORREF	textColor = 0x00000000;		// black foreground

	//	TransparentBlt(m_dc, inPt->x, inPt->y, dibW, dibH, dibDc, 0, 0, dibW, dibH, transColor);
	
		// Create a mask for the blit.
		HDC		transDC = ::CreateCompatibleDC(m_dc);
		HBITMAP	transBM = ::CreateBitmap(dibW, dibH, 1, 1, NULL);	// monochrome
		HBITMAP	oldBM;

		oldBM = (HBITMAP) ::SelectObject(transDC, transBM);
		SetBkColor(dibDc, transColor);

		BitBlt(transDC, 0, 0, dibW, dibH, dibDc, 0, 0, SRCCOPY);

		// make sure the text color is set back to black!
		SetTextColor(m_dc, textColor);

		BitBlt(m_dc, inPt->x, inPt->y, dibW, dibH, dibDc, 0, 0, SRCINVERT);
		BitBlt(m_dc, inPt->x, inPt->y, dibW, dibH, transDC, 0, 0, SRCAND);
		BitBlt(m_dc, inPt->x, inPt->y, dibW, dibH, dibDc, 0, 0, SRCINVERT);
		
		SelectObject(transDC, oldBM);
		DeleteObject(transBM);
		DeleteDC(transDC);
	}

	::DeleteDC(dibDc);
}

//
//	CopyDIB - Copy a DIB into the given rect of the offscreen buffer. We
//			never use this for transparents DIBs. Assume that the DIB really
//			starts at 0,0 and we offset our rect from that.
//
void View::CopyDIB(Dib *inDib, RECT *inRect)
{
	HDC		dibDC;
	RECT	dirtyRect;		// the rectangle we have soiled
	long	dibW; 			// width of dib
	long	dibH; 			// height of dib
	
	// sanity check on parameters	
	if ((inDib == NULL) or (inRect == NULL))
		return;
		
	// sanity check on inRect
	if ((inRect->left > VSCREEN_WIDTH) or (inRect->top > VSCREEN_HEIGHT))
		return;

	// see if we need to adjust the width of the dib
	if (inRect->right > VSCREEN_WIDTH)
		dibW = VSCREEN_WIDTH - inRect->left;
	else	
		dibW = inRect->right - inRect->left;            
	
	// do the same for the height of the DIB
	if (inRect->bottom > VSCREEN_HEIGHT)
		dibH = VSCREEN_HEIGHT - inRect->top;
	else
		dibH = inRect->bottom - inRect->top;
	
	//m_dirty = true; 
	dirtyRect.top = inRect->top;
	dirtyRect.left = inRect->left;
	dirtyRect.bottom = (int16) (inRect->top + dibH);
	dirtyRect.right = (int16) (inRect->left + dibW);
	
	if ((dirtyRect.top == 0) and 
		(dirtyRect.left == 0) and
		(dirtyRect.bottom > (VSCREEN_HEIGHT - 5)) and 
		(dirtyRect.right > (VSCREEN_WIDTH - 5)))
	{
		DirtyRect(&dirtyRect, true);				// dirty the whole screen
	}
	else
	{
		DirtyRect(&dirtyRect); 
	}

	dibDC = ::CreateCompatibleDC(m_dc);
	::SelectObject(dibDC, inDib->GetDib());

	BitBlt(m_dc, inRect->left, inRect->top, dibW, dibH, dibDC, inRect->left, inRect->top, SRCCOPY);
}

void View::DrawQTGraphic(QTGraphic *inQtg, POINT *inPt, bool inTrans)
{
	RECT	dirtyRect;
	long	graphic_height;
	long	graphic_width;

	// sanity check on pointer parameters	
	if ((inQtg == NULL) or (inPt == NULL))
		return;
		
	// sanity check on inPt
	if ((inPt->x > VSCREEN_WIDTH) or (inPt->y > VSCREEN_HEIGHT))
		return;

	// see if we need to adjust the height of the graphic
	graphic_height = inQtg->Height();
	if ((graphic_height + inPt->y) > VSCREEN_HEIGHT)
		graphic_height = VSCREEN_HEIGHT - inPt->y;
		
	// do the same for the width of the graphic
	graphic_width = inQtg->Width();
	if ((graphic_width + inPt->x) > VSCREEN_WIDTH)
		graphic_width = VSCREEN_WIDTH - inPt->x;

	dirtyRect.top = inPt->y;
	dirtyRect.left = inPt->x;
	dirtyRect.bottom = (int16) (inPt->y + graphic_height);
	dirtyRect.right = (int16) (inPt->x + graphic_width);
	if ((dirtyRect.top == 0) and 
		(dirtyRect.left == 0) and
		(dirtyRect.bottom > (VSCREEN_HEIGHT - 5)) and 
		(dirtyRect.right > (VSCREEN_WIDTH - 5)))
	{
		DirtyRect(&dirtyRect, true);				// dirty the whole screen
	}
	else
	{
		DirtyRect(&dirtyRect); 
	}

	inQtg->Draw(m_gworld, inPt, inTrans);
	//inQtg->Draw(m_gworld, &dirtyRect, inTrans);
}

void View::DrawQTGraphic(QTGraphic *inQtg, RECT *inRect)
{
	RECT	dirtyRect;
	long	graphic_height; 
	long	graphic_width;
	
	// sanity check on parameters	
	if ((inQtg == NULL) or (inRect == NULL))
		return;
		
	// sanity check on inRect
	if ((inRect->left > VSCREEN_WIDTH) or (inRect->top > VSCREEN_HEIGHT))
		return;

	// see if we need to adjust the width of the dib
	if (inRect->right > VSCREEN_WIDTH)
		graphic_width = VSCREEN_WIDTH - inRect->left;
	else	
		graphic_width = inRect->right - inRect->left;            
	
	// do the same for the height of the DIB
	if (inRect->bottom > VSCREEN_HEIGHT)
		graphic_height = VSCREEN_HEIGHT - inRect->top;
	else
		graphic_height = inRect->bottom - inRect->top;
	
	//m_dirty = true; 
	dirtyRect.top = inRect->top;
	dirtyRect.left = inRect->left;
	dirtyRect.bottom = (int16) (inRect->top + graphic_height);
	dirtyRect.right = (int16) (inRect->left + graphic_width);
	
	if ((dirtyRect.top == 0) and 
		(dirtyRect.left == 0) and
		(dirtyRect.bottom > (VSCREEN_HEIGHT - 5)) and 
		(dirtyRect.right > (VSCREEN_WIDTH - 5)))
	{
		DirtyRect(&dirtyRect, true);				// dirty the whole screen
	}
	else
	{
		DirtyRect(&dirtyRect); 
	}

	inQtg->Draw(m_gworld, &dirtyRect);
}

//
//	GetPixelAddress - Return the address of the desired pixel
//		in the bitmap. The inScreen boolean is used to indicate
//		which buffer you want. If inScreen is true then use m_screen
//		else use m_blippo.
//
HPSTR View::GetPixelAddress(bool inScreen, long inX, long inY)
{
	HPSTR	pixel_add = NULL; 
	long	width;
	
	// sanity check on arguments
	if ((inX < 0) or (inY < 0)
		or (inX >= Width()) or (inY >= Height()))
	{
		gLog.Log("Attempt to get out of range pixel address\n");
		return (NULL);
	}	
	
	width = StorageWidth();
	
	if (inScreen)
		pixel_add = m_screen_bits;
	else
		pixel_add = m_blippo_bits;
	
	if (m_topdown)
		pixel_add += (long) inY * width + (long) inX;
	else
		pixel_add += (long)(Height() - inY - 1) * width + (long) inX;
		
	return (pixel_add);
}

/* OUTDATED_STUFF

//
//	CopyDIBBits  - Copy the DIB bits to the offscreen buffer.
//
void View::CopyDIBBits(HPSTR inDstPtr, HPSTR inSrcPtr, long inWidth, long inHeight,
		long scanDst, long scanSrc)
{
	while (inHeight != 0)
	{
		memcpy(inDstPtr, inSrcPtr, inWidth);

		inSrcPtr -= scanSrc;
		inDstPtr -= scanDst;
		inHeight--;
	}
}

//
//	TransCopyDIBBits - Copy the DIB bits to the offscreen buffer and 
//			leave out the transparent color.
//
void View::TransCopyDIBBits(HPSTR inDstPtr, HPSTR inSrcPtr, long inWidth, long inHeight,
		long scanDst, long scanSrc, BYTE transColor) 
{
	HPSTR	theSrcPtr;
	HPSTR	theDstPtr;
	long	theWidth;
	BYTE	thePixel;
	
	while (inHeight != 0)
	{
		theSrcPtr = inSrcPtr;
		theDstPtr = inDstPtr;
		for (theWidth = 0; theWidth < inWidth; theWidth++)
		{
			thePixel = *theSrcPtr++;
			if (thePixel != transColor)
				*theDstPtr++ = thePixel;
			else
				theDstPtr++;
		}
		
		inSrcPtr -= scanSrc;
		inDstPtr -= scanDst;
		inHeight--;
	}
}
*/

//
//	BlippoCopy - If toBlippo is true, make a copy of the offscreen
//			buffer (m_screen) into the blippo buffer (m_blippo) else
//			do the inverse.
//
void View::BlippoCopy(bool toBlippo)
{
	if (not toBlippo)
	{
		gDebugLog.Log("Blippo");
		BitBlt(m_dc, 0, 0, VSCREEN_WIDTH, VSCREEN_HEIGHT, m_blippo_dc, 0, 0, SRCCOPY);
	}
	else
	{
		gDebugLog.Log("Unblippo");
		BitBlt(m_blippo_dc, 0, 0, VSCREEN_WIDTH, VSCREEN_HEIGHT, m_dc, 0, 0, SRCCOPY);
	}
}


//
//	Fade - Fade the screen in or out. The inTime parameter
//			determines how long the fade will take in tenths
//			of a second. 
//
//			not inFadeIn -> to black
//			inFadeIn  -> back up from black
//
//		If we are in 8 bit mode we do a normal palette fade.
//		If we are in 16, 24 or 32 bit mode we do a modified
//		palette fade. We keep changing the palette but then
//		we set the DIB color table with it.
//
void View::Fade(bool inFadeIn, long inTime, bool inMovieTime)
{ 
	HDC			hDC;
	HPALETTE	hPal;
	HPALETTE	oldPal;
	HPALETTE	fadePal;			// palette to fade with 
	float		fade_step;			// how long each iteration should take
	DWORD		start_time;
	DWORD		now_time;
	int			i, j;
	int			num_iterations; 	// number of iterations
	int			color_diff;			// amount to change color each iteration	
	RGBQUAD 	PalRGB[256];
	XPalette		Pal = { 0x300, 256 };
	XPalette		TargPal = { 0x300, 256 };
	PALETTEENTRY	PalDiff[256];

	if (m_pal == NULL)
	{
		gDebugLog.Log("Fade: <%s> but no palette to use", (inFadeIn? "IN": "OUT"));
		return; 					// can't fade without a palette
	}
			
	hPal = m_pal->GetPalHandle();
	if (hPal == NULL)
	{
		gDebugLog.Log("Fade: <%s> but no palette to use", (inFadeIn? "IN": "OUT"));
		return; 					// can't fade without a palette
	}

	if (inFadeIn and (not m_faded))
	{
		gDebugLog.Log("Fade: <IN> but already in!");
		return;						// already faded in, can't fade in again
    }
    else if ((not inFadeIn) and (m_faded))
    {
		gDebugLog.Log("Fade: <OUT> but already out!");
    	return;						// already faded out, can't fade out again
	}
	
	gDebugLog.Log("Fade: <%s>, <%ld>", (inFadeIn? "IN": "OUT"), inTime);

	if (inFadeIn)
	{
		gVariableManager.SetString("_faded", "0");
		m_faded = false;                          
	}
	else 
	{
		gVariableManager.SetString("_faded", "1");
		m_faded = true; 
		
		gCursorManager.ChangeCursor(NO_CURSOR);			// take away the cursor for fades
	}

	// do blinds if have more than 8 bits as too slow otherwise
	if (m_bit_depth > 8)
	{
		DoBlinds((not inFadeIn), inTime); 
		return;
	}

	if (not inFadeIn)  	// going to black
	{
		// get a copy of the palette
		::GetPaletteEntries(hPal, 0, 256, Pal.aEntries);
		
		for (i = 0; i < 256; i++)
		{
			Pal.aEntries[i].peFlags = PC_RESERVED;
			
			// need palette in RGB format for BitMap
			PalRGB[i].rgbBlue = Pal.aEntries[i].peBlue;
			PalRGB[i].rgbRed = Pal.aEntries[i].peRed;
			PalRGB[i].rgbGreen = Pal.aEntries[i].peGreen;
			PalRGB[i].rgbReserved = 0;
		}
        
        // the target palette is all black
	    for (i = 0; i < 256; i++)
	    {
	    	TargPal.aEntries[i].peRed = 0;
	    	TargPal.aEntries[i].peGreen = 0;
	    	TargPal.aEntries[i].peBlue = 0;
	    	TargPal.aEntries[i].peFlags = PC_RESERVED;
	    }
	}
	else 	// inFadeIn - come back up from black
	{
	    // start with an all black palette 
	    for (i = 0; i < 256; i++)
	    {
	    	Pal.aEntries[i].peRed = 0;
	    	Pal.aEntries[i].peGreen = 0;
	    	Pal.aEntries[i].peBlue = 0;
	    	Pal.aEntries[i].peFlags = PC_RESERVED;
            
            // need palette in RGB format for BitMap
			PalRGB[i].rgbBlue = 0;
			PalRGB[i].rgbRed = 0;
			PalRGB[i].rgbGreen = 0;
			PalRGB[i].rgbReserved = 0;
	    }
	    
	    // the target palette is whatever m_pal is
	    ::GetPaletteEntries(hPal, 0, 256, TargPal.aEntries);

		for (i = 0; i < 256; i++)
			TargPal.aEntries[i].peFlags = PC_RESERVED;
	}

	hDC = ::GetDC(hwndApp);
	
	if (m_bit_depth <= 8)
	{ 
		// create a palette from the entries we have for animation
		fadePal = ::CreatePalette((LOGPALETTE *) &Pal);
		
		// make it our current palette
		::UnrealizeObject(fadePal);
		oldPal = ::SelectPalette(hDC, fadePal, false);
		::RealizePalette(hDC); 
		
		// if we are fading in, we have to put the correct bits
		// onscreen with the black palette set
		// if the bit depth is > 8 we don't have to do this as we
		// will blast it up each iteration with BitBlt
		if (inFadeIn)
		{ 
			// make our color table match the screen palette (all black)
			::SetDIBColorTable(m_dc, 0, 256, PalRGB);         
			
			// now do a prelim draw of offscreen with black palette
			::BitBlt(hDC, 0, 0, VSCREEN_WIDTH, VSCREEN_HEIGHT, m_dc, 0, 0, SRCCOPY); 
		}
	}
	
	if (inTime == 0)
		num_iterations = 0;		// do it all at once
	else 
	{
		if (m_bit_depth <= 8)
			num_iterations = 60;	// good number for 8 bit 
		else
			num_iterations = 5;	// try this for 16 and 24 bit fades 
	}
    
    if (num_iterations > 0)
    {
		fade_step = ((float) (inTime * 100)) /  (float) num_iterations;

		color_diff = (256 / num_iterations) + 1;
		
		// set up table of color diff values 
		for (i = 0; i < 256; i++)
		{ 
			if (m_bit_depth <= 8)
			{
				PalDiff[i].peRed = color_diff;
				PalDiff[i].peGreen = color_diff;
				PalDiff[i].peBlue = color_diff;					
			}
			else
			{
				if (not inFadeIn)
				{
					PalDiff[i].peRed = (Pal.aEntries[i].peRed / num_iterations) + 1;
					PalDiff[i].peGreen = (Pal.aEntries[i].peGreen / num_iterations) + 1;
					PalDiff[i].peBlue = (Pal.aEntries[i].peBlue / num_iterations) + 1;
				}
				else
				{
					PalDiff[i].peRed = (TargPal.aEntries[i].peRed / num_iterations) + 1;
					PalDiff[i].peGreen = (TargPal.aEntries[i].peGreen / num_iterations) + 1;
					PalDiff[i].peBlue = (TargPal.aEntries[i].peBlue / num_iterations) + 1;					
				} 
			}
		}
	}
	
	start_time = ::timeGetTime();
	 
	// now animate our palette 
	for (j = 0; j < num_iterations; j++)
	{
		// Change the palette. For bit depths greater than 8 we keep
		//	an array of RGB values in sync with the palette so we can
		// 	call SetDIBColorTable().
		if (not inFadeIn) 				// going to black
		{
			for (i = 1; i < 255; i++)
			{
				if (Pal.aEntries[i].peRed > PalDiff[i].peRed)
					Pal.aEntries[i].peRed -= PalDiff[i].peRed;
				else if (Pal.aEntries[i].peRed > 0)
					Pal.aEntries[i].peRed--;
					
				if (Pal.aEntries[i].peGreen > PalDiff[i].peGreen)
					Pal.aEntries[i].peGreen -= PalDiff[i].peGreen;
				else if (Pal.aEntries[i].peGreen > 0)
					Pal.aEntries[i].peGreen--;
		
				if (Pal.aEntries[i].peBlue > PalDiff[i].peBlue)
					Pal.aEntries[i].peBlue -= PalDiff[i].peBlue;
				else if (Pal.aEntries[i].peBlue > 0)
					Pal.aEntries[i].peBlue--;
				
				if (m_bit_depth > 8)
				{	
					PalRGB[i].rgbBlue = Pal.aEntries[i].peBlue;
					PalRGB[i].rgbRed = Pal.aEntries[i].peRed;
					PalRGB[i].rgbGreen = Pal.aEntries[i].peGreen;
					PalRGB[i].rgbReserved = 0; 
				}
			} 
		}
		else 	/* inFadeIn - back up from black */
		{ 
			for (i = 0; i < 256; i++)
			{
				if (Pal.aEntries[i].peRed < (TargPal.aEntries[i].peRed - PalDiff[i].peRed))
					Pal.aEntries[i].peRed += PalDiff[i].peRed;
				else if (Pal.aEntries[i].peRed < TargPal.aEntries[i].peRed)
					Pal.aEntries[i].peRed++;
					
				if (Pal.aEntries[i].peGreen < (TargPal.aEntries[i].peGreen - PalDiff[i].peGreen))
					Pal.aEntries[i].peGreen += PalDiff[i].peGreen;
				else if (Pal.aEntries[i].peGreen < TargPal.aEntries[i].peGreen)
					Pal.aEntries[i].peGreen++;
		
				if (Pal.aEntries[i].peBlue < (TargPal.aEntries[i].peBlue - PalDiff[i].peBlue))
					Pal.aEntries[i].peBlue += PalDiff[i].peBlue;
				else if (Pal.aEntries[i].peBlue < TargPal.aEntries[i].peBlue)
					Pal.aEntries[i].peBlue++;
                
                if (m_bit_depth > 8)
                {
					PalRGB[i].rgbBlue = Pal.aEntries[i].peBlue;
					PalRGB[i].rgbRed = Pal.aEntries[i].peRed;
					PalRGB[i].rgbGreen = Pal.aEntries[i].peGreen;
					PalRGB[i].rgbReserved = 0;
				} 
			} 
		}		
		
		if (m_bit_depth > 8)
		{
			// set the DIB color table from the palette and then
			//	blast the offscreen buffer to the screen
			::SetDIBColorTable(m_dc, 0, 256, PalRGB);
			::BitBlt(hDC, 0, 0, VSCREEN_WIDTH, VSCREEN_HEIGHT, m_dc, 0, 0, SRCCOPY);
		}
		else
			// just animate the palette which will redraw the screen
			::AnimatePalette(fadePal, 0, 256, Pal.aEntries);
			
		if (inMovieTime)
		{
			gVideoManager.Idle();
			gAudioManager.Idle();
		}
			
		now_time = ::timeGetTime();

		while ((now_time - start_time) < fade_step)
			now_time = ::timeGetTime();	
			
		start_time = ::timeGetTime();
	}
    
    // now, no matter what bit depth we are at we have
    // to ensure that the DIB color table is in sync
	// with the target palette
	for (i = 0; i < 256; i++)
	{
		PalRGB[i].rgbBlue = TargPal.aEntries[i].peBlue;
		PalRGB[i].rgbRed = TargPal.aEntries[i].peRed;
		PalRGB[i].rgbGreen = TargPal.aEntries[i].peGreen;
		PalRGB[i].rgbReserved = 0;
	}
	
	::SetDIBColorTable(m_dc, 0, 256, PalRGB);
	
	if (not inFadeIn) 	// go all the way to black
	{	
		// make sure we are all the way to black
		if (m_bit_depth > 8)
			::BitBlt(hDC, 0, 0, VSCREEN_WIDTH, VSCREEN_HEIGHT, m_dc, 0, 0, SRCCOPY); 
		else
			::AnimatePalette(fadePal, 0, 256, TargPal.aEntries);

		BlackScreen();	// wipe out what is on the screen
						// while preserving the contents of m_screen 
						
		// now we have to set the color table back to m_pal
		SetPalette(m_pal);
	}
	else				// make sure m_screen is all on with correct palette
		Draw(true);		// m_pal already has the same entries as TargPal (as TargPal was created from m_pal)
		
	::SelectPalette(hDC, oldPal, false);
	::ReleaseDC(hwndApp, hDC);
	
	if (m_bit_depth <= 8)
		::DeleteObject(fadePal); 
		
	if (not m_faded)
		gCursorManager.CheckCursor();		// make sure the cursor comes back as it should
}


//
//	DoEffect - Take what is in the blippo buffer and do the 
//		specified visual effect over the specified amount of
//		time.
//
void View::DoEffect(Effect inEffect, long inTime)
{ 
	
	if ((inEffect == transNONE) or (inTime == 0))
	{
		// just blast blippo back into the offscreen world
		// and then blast that back to the screen
		
		BlippoCopy(false);
		Draw(true); 
	}
	else
	{ 
		switch (inEffect)
		{
			case transLEFT:
			case transRIGHT:
			case transUP:
			case transDOWN:
				DoWipe(inEffect, inTime);
				break;
				
			default:
				BlippoCopy(false);
				Draw(true);
				break;
		}
	}	
}

//
//	DoWipe - Do the wipe effect.
//
void View::DoWipe(Effect inEffect, long inTime)
{
	HDC			h_dc; 
	HPALETTE	hPal;
	HPALETTE	h_old_pal;
	DWORD		start_time;
	DWORD  		now_time;
	RECT		dest_rect;      
	float		effect_step;
	int16		pixels_to_move;
	int16		pixels_per_loop;
	int16		h_delta;
	int16		v_delta;
	int			num_iterations;
	int			i;
    
    if (m_pal == NULL)
    	return;

	hPal = m_pal->GetPalHandle();
	if (hPal == NULL)
		return;
    	
	if ((inEffect == transLEFT) or (inEffect == transRIGHT))
		pixels_to_move = VSCREEN_WIDTH;
	else
		pixels_to_move = VSCREEN_HEIGHT;
			
	num_iterations = 40;
		
	effect_step = ((float) (inTime * 100)) / (float) num_iterations;

	gDebugLog.Log("Wipe: each step should take <%d> msecs", effect_step);

	pixels_per_loop = (pixels_to_move / num_iterations) + 1;
	
	dest_rect.top = 0;
	dest_rect.left = 0;
	dest_rect.bottom = VSCREEN_HEIGHT;
	dest_rect.right = VSCREEN_WIDTH;
	
	h_delta = 0;
	v_delta = 0;
	
	switch (inEffect)
	{
		case transLEFT:
			dest_rect.left += (pixels_to_move - pixels_per_loop);
			h_delta = -pixels_per_loop;
			break;
		case transRIGHT:
			dest_rect.right -= (pixels_to_move - pixels_per_loop);
			h_delta = pixels_per_loop;
			break;
		case transDOWN:
			dest_rect.bottom -= (pixels_to_move - pixels_per_loop);
			v_delta = pixels_per_loop;
			break;
		case transUP:
			dest_rect.top += (pixels_to_move - pixels_per_loop);
			v_delta = -pixels_per_loop;
			break;
	}
		 
	h_dc = ::GetDC(hwndApp); 
	::UnrealizeObject(hPal);
	h_old_pal = ::SelectPalette(h_dc, hPal, false);
	::RealizePalette(h_dc);
	
	gCursorManager.ChangeCursor(NO_CURSOR);
		
	start_time = ::timeGetTime();
	
	for (i = 0; i < num_iterations; i++)
	{
		// we are all set for the first blit 
		::BitBlt(h_dc, dest_rect.left, dest_rect.top, (dest_rect.right - dest_rect.left), (dest_rect.bottom - dest_rect.top),
			m_dc, dest_rect.left, dest_rect.top, SRCCOPY);
		
		// see if we should wait for a while
		now_time = ::timeGetTime();
		
		while ((now_time - start_time) < effect_step)
		{
			gVideoManager.Idle();
			gAudioManager.Idle();
			
			now_time = ::timeGetTime();
		}
			
        gVideoManager.Idle();
        gAudioManager.Idle();
        
        // adjust our clipping rectangle 
        ::OffsetRect(&dest_rect, h_delta, v_delta);
        
        start_time = ::timeGetTime();
	}
    
    ::SelectPalette(h_dc, h_old_pal, false);
    ::ReleaseDC(hwndApp, h_dc);
    
    gCursorManager.CheckCursor();
} 

void View::DoBlinds(bool inClose, long inTime)
{
	HDC		h_dc;
	DWORD	start_time;
	DWORD	now_time;
	float	effect_step;
	int		num_iterations;
	int		top;
	int		num_slats;
	int		rect_height;
	int		i, j;
	
	gCursorManager.ChangeCursor(NO_CURSOR);	// hide cursor
	
	num_slats = 20;				// good number
	num_iterations = 12;		// another one 
	
	effect_step = ((float) (inTime * 100)) / (float) num_iterations;
	
	// cbo_note - why don't we use the parameter?
	effect_step = 10;			// make them really fast no matter what
	
	if (inClose)
		ClearScreen(0);			// set the offscreen to black	
	
	rect_height = VSCREEN_HEIGHT/num_slats/num_iterations;

	h_dc = ::GetDC(hwndApp); 
	
	start_time = ::timeGetTime();
	
	for (i = 0; i < num_iterations; i++)
	{
		for (j = 1; j <= num_slats; j++)
		{
			if (inClose)
				top = ((j * (VSCREEN_HEIGHT/num_slats)) - (i * rect_height)) - rect_height;
			else
				top = ((j - 1) * (VSCREEN_HEIGHT/num_slats)) + (i * rect_height);

			::BitBlt(h_dc, 0, top, VSCREEN_WIDTH, rect_height, m_dc, 0, top, SRCCOPY);
		}
		
		now_time = ::timeGetTime();
		while ((now_time - start_time) < effect_step)
		{
			gVideoManager.Idle();
			gAudioManager.Idle();
			
			now_time = ::timeGetTime();
		}
		
		start_time = ::timeGetTime();
	}
	
	gCursorManager.CheckCursor();
		
	::ReleaseDC(hwndApp, h_dc);
} 

    
//
//	StringToEffect - 
//
Effect View::StringToEffect(TString &effectString)
{
	TString		tmpStr = effectString;

	tmpStr.MakeLower();

	if (tmpStr.IsEmpty())
     	return transNONE;
	else if (tmpStr.Equal("left"))
    	return transLEFT;
    else if (tmpStr.Equal("none")) 
    	return transNONE;
    else if (tmpStr.Equal("right")) 
    	return transRIGHT;
    else if (tmpStr.Equal("up")) 
    	return transUP;
    else if (tmpStr.Equal("down")) 
    	return transDOWN;
    else if (tmpStr.Equal("blinds")) 
    	return transBLINDS;
    else if (tmpStr.Equal("xfade")) 
    	return transXFADE;
    else if (tmpStr.Equal("gfade")) 
    	return transGFADE;
    else if (tmpStr.Equal("pushleft")) 
    	return pushLEFT;
    else if (tmpStr.Equal("pushright")) 
    	return pushRIGHT;
    else if (tmpStr.Equal("pushup")) 
    	return pushUP;
    else if (tmpStr.Equal("pushdown")) 
    	return pushDOWN;
    else 
    {
        gDebugLog.Log("Error: Unknown effect: %s.", (const char *) effectString); 
        return transNONE;
    }
}

/*
 $Log$
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

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.8  2000/08/08 19:03:41  chuck
 no message

 Revision 1.7  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.6  2000/03/01 15:46:55  chuck
 no message

 Revision 1.5  2000/02/02 15:15:33  chuck
 no message

 Revision 1.4  1999/12/16 17:17:36  chuck
 no message

 Revision 1.3  1999/09/24 20:03:26  chuck
 Clean up

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
