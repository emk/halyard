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
// View.h : 
//

#if !defined (_View_h_)
#define _View_h_

#include "TCommon.h"
#include "TRect.h"
#include "TPoint.h"
//#include "LCommon.h"
//#include "Graphics.h"
#include "Dib.h"
#include "LPalette.h"
#include "QTGraphic.h"

#define VSCREEN_WIDTH  		640
#define VSCREEN_HEIGHT		480
#define MAX_RECTS			20

typedef struct
{
	WORD 			Version;
	WORD 			NumberOfEntries;
	PALETTEENTRY 	aEntries[256];
} XPalette;

//////////
// Visual Effect Enumeration
//
enum Effect 
{
    transNONE = 0,
    transLEFT,
    transRIGHT,
    transUP,
    transDOWN,
    transBLINDS,
    transXFADE,
    transGFADE,
    pushLEFT,
    pushRIGHT,
    pushUP,
    pushDOWN
};

/*-----------------------------------------------------------------

CLASS
    View

	Graphical screen viewport with offscreen and blippo buffers.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class View : public TObject
{
    public:
    	//////////
		// Constructor.
		//
		View();
    	
		//////////
		// Destructor.
		//
		~View();

		//////////
		// Initialization.  Create the offscreen buffer.  Create the blippo buffer.
		// Create a Mac GWorld for the QuickTime Image components.
		//
		bool		Init(void);
    	
    	//////////
		// Get the graphical device context for the offscreen world.
		//
		// [out] return - DC for offscreen world
		//
		HDC			GetDC(void);
    	
		//////////
		// Set our palette. We reset the color table of our offscreen world and
		// remember the HPALETTE to set it when we want to blast to the screen.
		//
		// [in] inPal - LPallete to use for our palette
		//
		void		SetPalette(LPalette *inPal);
    	
		//////////
		// Get the current palette.
		//
		// [out] return - the current palette
		//
		LPalette	*GetPalette(void) { return (m_pal); }
    	
		//////////
		// Blast the offscreen world to the screen.  Update only rectangular portions
		// of the screen that are "dirty" unless inForce is set.
		//
		// [in_optional] inForce - force an entire screen redraw (default false)
		//
		void		Draw(bool inForce = false);
    	
		//////////
		// Copy the offscreen world to the blippo world.
		//
		void		Blippo(void);
    	
		//////////
		// Restore the saved bitmap created by blippo with an optional Effect.
		//
		// [in] inEffect - the desired visual Effect
		// [in] inTime - amount of time for the effect
		//
		void		UnBlippo(TString &inEffect, long inTime);
    	
		//////////
		// Switch to lock mode where we won't blast the offscreen stuff to the screen.
		//
		// [in_optional] inClear - if true, clear the screen before locking it (default false)
		//
		void		Lock(bool inClear = false);
    	
		//////////
		// Unlock the scrren (after a Lock) with an optional effect.
		//
		// [in] inEffect - the desired visual Effect
		// [in] inTime - amount of time for the effect
		//
		void		Unlock(TString &inEffect, long inTime);
    	
		//////////
		// Clear the screen to the given color.
		//
		// [in] inColor - color used to fill the screen
		//
		void		ClearScreen(int inColor); 
    	
		//////////
		// Copy a DIB into the offscreen buffer. We always copy the entire DIB.
		//
		// [in] inDib - DIB to copy to offscreen buffer
		// [in] inPt - a Win32 POINT specifying the size of the DIB
		// [in] inTransColor - IGNORED (white is used for transparency color)
		// [in] inTrans - is the DIB transparent?
		//
		void		CopyDIB(Dib *inDib, POINT *inPt, BYTE inTransColor, bool inTrans);
    	
		//////////
		// Copy a DIB into the given rect of the offscreen buffer. We never use this
		// for transparents DIBs. Assume that the DIB really starts at 0,0 and
		// we offset our rect from that.
		//
		// [in] inDib - DIB to copy to offscreen buffer
		// [in] inRect - a Win32 RECT specifying the rectangle to copy into
		//
		void		CopyDIB(Dib *inDib, RECT *inRect);
		
		//////////
		// Draw a Quicktime graphic to the screen.
		//
		// [in] inQtg - QTGraphic to be drawn
		// [in] inPt - top-left coordinate of the desired screen location
		// [in] inTrans - if true, mark the graphic as transparent
		//
		void		DrawQTGraphic(QTGraphic *inQtg, POINT *inPt, bool inTrans);
		
		//////////
		// Draw a Quicktime graphic to the screen.
		//
		// [in] inQtg - QTGraphic to be drawn
		// [in] inRect - Win32 RECT specifying screen location of the graphic
		//
		void		DrawQTGraphic(QTGraphic *inQtg, RECT *inRect);
 		
    	//////////
		// Fade the screen in or out.
		//
		// [in] inFadeIn - true = fade from black, false = fade to black
		// [in] inTime - how long the fade will take in 1/10 seconds
		// [in] inMovieTime - if true, give idle time to gVideoManager and gAudioManager
		//
		void		Fade(bool inFadeIn, long inTime, bool inMovieTime);
    	
		//////////
		// Is the view faded?
		//
		// [out] return - true if the view is faded, false otherwise
		//
		bool		Faded(void) { return (m_faded); }
    		
    	//////////
		// Fill the screen with black (not the offscreen buffer!).
		//
		void		BlackScreen(void);
		
		//////////
		// Return a COLORREF with info about the given color in the	current palette.
		//
		// [in] inColor - the color
		//
		COLORREF	GetColor(int inColor);
   	    
		//////////
		// Get the current bit depth.
		//
		// [out] return - the bit depth
		//
		int			BitDepth(void) { return (m_bit_depth); }
   	    
		//////////
		// Get the height of the view.
		//
		// [out] return - the height of the view
		//
    	long		Height(void) { return (m_bmp_info.biHeight); }
    	
		//////////
		// Get the width of the view.
		//
		// [out] return - the width of the view
		//
		long 		Width(void)	{ return (m_bmp_info.biWidth); }
    	
		//////////
		// Get storage width of the bitmap. 
		//
		// [out] return - the storage width for the associated bitmap
		//
		long		StorageWidth(void) { return ((Width() + 3) & ~3); }
    		
    	//////////
		// The given rectangle is dirty. Put it in the list of dirty rectangles to
		// ensure it gets drawn the	next time.
		//
		// [in] inRect - a TRect specifying the dirty region
		//
		void		DirtyRect(TRect *inRect);	
		
		//////////
		// The given rectangle is dirty. Put it in the list of dirty rectangles to
		// ensure it gets drawn the	next time.<br>
		// Note: If inRect==NULL, the whole screen is marked as dirty.
		//
		// [in] inRect - a Win32 RECT specifying the dirty region
		// [in_optional] inWholeScreen - if true, mark the whole screen dirty (default false)
		//
		void		DirtyRect(RECT *inRect, bool inWholeScreen = false);
    	
	private:
		//////////
		// Device context.
		//
		HDC			m_dc;
		
		//////////
		// Old bitmap.
		//
		HBITMAP		m_old_bmp;
		
		//////////
		// Offscreen drawing.
		//
		HBITMAP		m_screen;		
		
		//////////
		// Bits of m_screen.
		//
		LPSTR		m_screen_bits;	
		
		//////////
		// Device context for blippo.
		//
		HDC			m_blippo_dc;	
		
		//////////
		// Blippo buffer.
		//
		HBITMAP		m_blippo;		
		
		//////////
		// Old blippo BMP.
		//
		HBITMAP		m_old_blippo_bmp;
		
		//////////
		// Bits of m_blippo.
		//
		LPSTR		m_blippo_bits;	
		
		//////////
		// Current palette .
		//
		LPalette	*m_pal;			
		
		//////////
		// GWorld for offscreen buffer.
		//
		GWorldPtr	m_gworld;		

		//////////
		// Information about our bitmap.
		//
		BITMAPINFOHEADER	m_bmp_info;
		
		//////////
		// Bits per pixel on the screen.
		//
		int			m_bit_depth;	
		
		//////////
		// Bits per pixel in dib.
		//
		int			m_dib_bit_depth;
		
		//////////
		// ??? (always false)
		//
		bool		m_topdown;
		
		//////////
		// Do we have a screen shot in the blippo buffer?
		//
		bool		m_blippoed;		
		
		//////////
		// When true, don't blast to the screen.
		//
		bool		m_locked;		
        
		//////////
		// Is the view faded?
		//
		bool		m_faded; 
		
		//////////
		// Do we have a new palette?
		//
		bool		m_new_pal;
        
		//////////
		// Is a screen update needed?
		//
        bool		m_dirty;		
        
		//////////
		// Number of dirty rectangles.
		//
		int16		m_num_rects;	
        
		//////////
		// Dirty rectangles.
		//
		RECT		m_rects[MAX_RECTS];
        
		//////////
		// Take what is in the blippo buffer and do the specified visual effect
		// over the specified amount of time.
		//
		// [in] inEffect - the visual effect desired
		// [in] inTime - amount of time for the effect
		//
        void		DoEffect(Effect inEffect, long inTime);

		//////////
		// Do the wipe effect.
		//
		// [in] inEffect - used to specify the direction of the wipe
		// [in] inTime - amount of time for the effect
		//
        void		DoWipe(Effect inEffect, long inTime);
        
		//////////
		// Do the blinds effect.
		//
		// [in] inClose - if true, close blinds otherwise open blinds
		// [in] inTime - amount of time for the effect (CURRENTLY IGNORED)
		//
		void		DoBlinds(bool inClose, long inTime);
        
		//////////
		// Return the address of the desired pixel in the bitmap.
		//
		// [in] inScreen - used to indicate which buffer 
		//				   (true = m_screen, false = m_blippo)
		// [in] inX - X-Coordinate
		// [in] inY - Y-Coordinate
		// [out] return - the pixel address
		//
		HPSTR		GetPixelAddress(bool inScreen, long inX, long inY);

		//////////
		// If toBlippo is true, make a copy of the offscreen buffer (m_screen)
		// into the blippo buffer (m_blippo) else do the inverse.
		//
		// [in] toBlipppo - indicates direction of copy
		//
		void		BlippoCopy(bool toBlipppo);
		
		//////////
		// Convert a string to an Effect.
		//
		// [in] effectString - effect string
		// [out] return - the Effect
		//
		Effect 		StringToEffect(TString &effectString);

		//LPalette	*m_save_pal;		// saved palette 
		//HPALETTE	m_fade_pal;		// palette to use when faded

/* OUTDATED_STUFF

		void		CopyDIBBits(HPSTR inDstPtr, HPSTR inSrcPtr, long inWidth, long inHeight,
						long scanDst, long scanSrc);
		void		TransCopyDIBBits(HPSTR inDstPtr, HPSTR inSrcPtr, long inWidth, long inHeight,
						long scanDst, long scanSrc, BYTE transColor);		        
*/
};

#endif // _View_h_

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

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.5  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.4  2000/03/01 15:46:55  chuck
 no message

 Revision 1.3  2000/02/02 15:15:33  chuck
 no message

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
