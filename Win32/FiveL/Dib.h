// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

#if !defined (_Dib_h_)
#define _Dib_h_

#include <mmsystem.h>
#include "DibLib.h"

/*-----------------------------------------------------------------

CLASS
    Dib

	A class that encapsulates a Device-Independent Bitmap.  Uses 
	structures and functions from the DibLib library.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class Dib : public TObject
{
	public:
		//////////
		// Constructor.
		//
		Dib();

		//////////
		// Constructor.  Creates a Dib from the given handle.
		//
		// [in] inDIB - a handle to a DIB
		//
		Dib(HDIB inDIB);
		
		//////////
		// Destructor
		//
		~Dib();
		
		//////////
		// Create a Dib given a DIB handle.
		//
		// [in] inDIB - DIB handle
		//
		void	Create(HDIB inDIB); 
		
		// DEPRECATED 
		//
		//HPSTR	GetPixelAddress(long x, long y);
		
		//////////
		// ??? Get DIB bits ???
		//
		// [out] return - ???
		//
		void	*GetBits(void) { return (m_dib_bits); }
		
		//////////
		// Get DIB header info.
		//
		// [out] return - the DIB header
		//
		LPBITMAPINFOHEADER GetHeader(void) { return (m_dib_info); }
		
		//////////
		// Get DIB width.
		//
		// [out] return - the width
		//
		long	Width(void) { return (DibWidth(m_dib)); }
		
		//////////
		// Get DIB height.
		//
		// [out] return - the height
		//
		long	Height(void) { return (DibHeight(m_dib)); }
		
		//////////
		// Get DIB bit count.
		//
		// [out] return - the bit count
		//
		long	BitCount(void) { return (DibBitCount(m_dib)); }
		
		//////////
		// Get DIB size.
		//
		// [out] return - the size
		//
		long	Size(void) { return (DibBitsSize(m_dib)); }
		
		// DEPRECATED
		//
		//long		NumColors(void);
		
		//////////
		// Get DIB palette size.
		//
		// [out] return - the palette size
		//
		long		PaletteSize(void);
		
		//////////
		// Get DIB palette.
		//
		// [out] return - the palette
		//
		HPALETTE	GetPalette(void);
		
		//////////
		// Get DIB handle.
		//
		// [out] return - DIB handle
		//
		HBITMAP		GetDib(void) { return (DibBitmapHandle(m_dib)); }
		
		//////////
		// Get palette entry.
		//
		// [in] entry - entry in palette
		// [out] return - a Win32 COLORREF which specifies a 32-bit RGB color
		//
		COLORREF	GetPaletteEntry(int32 entry);
		
    private:
    	//////////
		// DIB handle.
		//
		HANDLE		m_dib;
    	
		//////////
		// DIB header info.
		//
		LPBITMAPINFOHEADER	m_dib_info;
    	
		//////////
		// ???
		//
		void *		m_dib_bits; 	
};

#endif // _Dib_h_

/*
 $Log$
 Revision 1.2  2002/07/25 22:25:36  emk
   * Made new CryptStream auto_ptr code work under Windows.
   * PURIFY: Fixed memory leak in TBTree::Add of duplicate node.  We now
     notify the user if there are duplicate cards, macros, etc.
   * PURIFY: Fixed memory leak in TBTree destructor.
   * PURIFY: Fixed memory leak in ConfigManager destructor.
   * PURIFY: Fixed memory leaks when deleting DIBs.
   * PURIFY: Made sure we deleted offscreen GWorld when exiting.
   * PURIFY: Fixed memory leak in LBrowser.
   * PURIFY: Fixed memory leak in LFileBundle.
   * PURIFY: Fixed uninitialized memory reads when View methods were
     called before View::Init.
   * PURIFY: Made View::Draw a no-op before View::Init is called.
     (It seems that Windows causes us to call Draw too early.)

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
