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

#if !defined (_LPicture_h_)
#define _LPicture_h_

#include "TCommon.h"
#include "TRect.h"
#include "TPoint.h"

#include "LResource.h"
#include "Dib.h"
#include "QTGraphic.h"

/*-----------------------------------------------------------------

CLASS
    LPicture

	A picture resource.  Handles loading, unloading, and drawing the
	picture (using gView).

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LPicture : public LResource 
{
    public:
        //////////
		// Constructor.  Load the picture, update its size and priority.
		//
		// [in] inName - name of the picture
		//
		LPicture(const char *inName);
        
		//////////
		// Destructor.
		//
		virtual 		~LPicture();

        //////////
		// Load the picture into memory.
		//
		virtual void   _Load(void);
        
		//////////
		// Unload the picture from memory.
		//
		virtual void   _Purge(void);

        //////////
		// Draw the picture to the screen at the specified location. 
		//
		// [in] inPt - top left corner of desired screen location
		// [in_optional] inMatte - if true, 1 is used as the transparent color (default false)
		//
		void    		Draw(TPoint &inPt, bool inMatte = false);
        
		//////////
		// Draw the picture in the given rectangle.
		//
		// [in] inRect - TRect specifying where the picture should be drawn
		//
		void    		Draw(TRect &inRect);
        
		//////////
		// Draw the highlight picture of a graphic. Then draw ourself again.
		//
		// [in] inPt - top-left coordinate of desired screen location
		// [in_optional] inMatte - if true, 1 is used as the transparent color (default true)
		//
		void    		Hilite(TPoint &inPt, bool inMatte = true);
        
		//////////
		// Find the hilite picture, if it exists.
		//
		// [out] return - the highlight picture for this picture, or NULL
		//				if no hilight picture could be found
		//
		LPicture 		*GetHilitePicture(void);

        //void    		SetPalette(bool graphPal = true);

        //////////
		// Get the origin of this picture.
		//
		// [out] return - the origin of this picture
		//
		TPoint    		GetOrigin();
        
		//////////
		// Get the current rectangular bounds for this picture.
		//
		// [out] return - the rectangular bounds
		//
		TRect    		GetBounds();
		
		//////////
		// Get the palette for this picture.
		//
		// [out] return - a Win32 palette handle 
		//
		HPALETTE		GetPalette(void);
		
		//////////
		// Get the color table for this picture.
		//
		// [out] return - a Quicktime color table handle
		//
		CTabHandle		GetColorTable(void);
		
	private:
		//////////
		// The actual meat of loading the picture. Open the LPicture file and 
		// turn it into a DIB.
		//
		void			LoadDIB(void);
		
		//////////
		// Load the picture using QuickTime.
		//
		void			LoadQTGraphic(void);

		//////////
		// Make sure m_Height and m_Width are up to date.  (This was
		// refactored out of the code; ideally, we'd set these variables
		// up correctly *once* instead of updating them repeatedly.)
		//
		void			UpdateHeightAndWidth();

		//////////
		// Full path to this picture.
		//
		TString			m_FullPath;
        
		//////////
		// Top-left point where picture should be drawn.
		//
		TPoint			m_Origin;
		
		//////////
		// Width of picture.
		//
		int32			m_Width;
		
		//////////
		// Height of picture.
		//
		int32			m_Height;

		//////////
		// Use native bitmap functions to load picture?<br>
		// (DISABLED - .BMP files are also loaded using QT now)
		//
		bool			m_NativeBmp;
		
		//////////
		// Associated DIB.
		//
		Dib				*m_Dib;
		
		//////////
		// QTGraphic for this picture.
		//
		QTGraphic		*m_Qtg;

        //int         	width, height;
        //int         	mask_loaded;
        
        
        //bool			m_have_size;
		//bool			m_native_bmp;

		//Dib				*m_dib;
		//QTGraphic		*m_qtg;

		//bool			m_MaskLoaded;
		//bool			m_HaveSize;
};

/*-----------------------------------------------------------------

CLASS
    LPictureManager

	Manages a set of LPicture objects.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LPictureManager  : public LResourceManager
{
public:
	//////////
	// Constructor.
	//
	LPictureManager() {}
	
	//////////
	// Destructor.
	//
	~LPictureManager() {}

	//////////
	// Find an LPicture in our resource list (actually a tree).
	//
	// [in] inName - name of the picture to find
	// [out] return - an LPicture, or NULL if not found
	//
	LPicture		*GetPicture(TString &inName);
};

#endif // _LPicture_h_

/*
 $Log$
 Revision 1.2  2002/06/20 16:32:55  emk
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

 Revision 1.1.10.1  2002/06/19 15:47:31  emk
 3.3.4.9 - Fixed _Origin_X, _Origin_Y bugs, and added a default style
 parameter to the DEFSTYLE command.  See TVersion.h log message for more
 detailed documentation.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  2000/02/02 15:15:32  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
