//
//	CPicture.h
//

#ifndef _H_CPICTURE
#define _H_CPICTURE

#include "Mac5L.h"

#include "CResource.h"
#include "CRect.h"
#include "CGWorld.h"

/*	We're trading memory for speed here. If we need to decrease the memory we could do
	masking as the pic is drawn (would do away with the PixMapHandles & the BitMap).
	
	BITMAP is a macro used to get the bitmap that we want to draw into. it is
	used when calling DrawPic so that we can draw into either an offscreen bitmap
	or directly onto the screen. Offscreen drawing is used when drawing the 'normal'
	button - when the button is highlighted we draw directly to the screen (for speed).
	See 'CTouchZone.cp'.
	
 	w can be a WindowPtr, a DialogPtr, or a GWorldPtr. In any of these
 	cases, it returns the correct "bitmap" to send to DrawPic() in the
 	destination bitmap parameter.	*/
#define BITMAP(w)		(((GrafPtr)(w))->portBits)

class CPicture : public CResource 
{

    private:
		int16			mWidth, mHeight;
		int16			mHiWidth, mHiHeight;
		PicHandle 		mPicture;				// handle to picture
		PicHandle		mHiPicture;				// handle to hilite picture
		CString			mPicName;				// name of the picture
		bool			mMatte;					// draw picture matted
		
		// We use the following Bitmaps for drawing matted pictures. LoadMatte creates the
		// bitmap of the picture, along with a mask bitmap whose bit is set for each non-white
		// pixel.
		PixMapHandle	mPicBits;				// Bitmap representation of above
		CGWorld			*mPicWorld;				// Offscreen GWorld that above PixMap belongs to
		
		PixMapHandle	mHiPicBits;
		CGWorld			*mHiPicWorld;
		
		void 			LoadPicFile(bool isHiPic);
		void 			SetMatte(const PicHandle inPicture, bool isHiPic);
        void			TossMatte(void);
        void    		LoadMatte(const bool isHilite);

    public:
        				CPicture(const char *inName, const char *inBaseName, bool inMatte);
        virtual 		~CPicture();
        OVERRIDE void   _Load(void);
        OVERRIDE void   _Purge(void);
        void    		DrawPic(Point &loc, BitMap &destBits, Boolean matte = FALSE, Boolean direct = FALSE);
	    void    		DrawHiPic(Point &loc, BitMap &destBits, Boolean matte = FALSE, Boolean direct = FALSE);
        void    		DrawPic(Rect &loc, BitMap &destBits,  Boolean matte = FALSE, Boolean direct = FALSE);
        void    		Hilite(Point &loc, BitMap &destBits, Boolean matte = FALSE, Boolean direct = FALSE);
		Rect			GetPictureRect(void);

        PicHandle		GetPicHandle() { return mPicture; }
        PicHandle		GetHiPicHandle() { return mHiPicture; }
};

//void 		UpdatePalette(const char *inName, bool inGraphics);
CPicture	*GetPicture(const char *name, bool matte = false);

#endif
