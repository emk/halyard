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
// LPalette.h : 
//

#if !defined (_LPalette_h_)
#define _LPalette_h_

#include "LResource.h"
#include "QTGraphic.h"

/*-----------------------------------------------------------------

CLASS
    LPalette

	A class for creating a palette from a BMP file.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LPalette : public LResource
{
	public:
		//////////
		// Constructor.
		//
		// [in] name - name of the BMP palette file without file extenstion (e.g. pal3)
		//
		LPalette(const char *name);

		//////////
		// Destructor
		//
		~LPalette();
			
		// DEPRECATED
		//
		//void			SetPalette(bool inGraphPal = true);
		
		//////////
		// Get the palette name.
		//
		// [out] return - name of the palette
		//
		const char *	GetName(void) { return ((const char *) m_PalName); }
		
		//////////
		// Get a Win32 palette handle.
		//
		// [out] return - Win32 palette handle
		//
		HPALETTE		GetPalHandle(void) { return (m_pal); }
		
		//////////
		// Get a Quicktime color table handle.
		//
		// [out] return - QT colortable handle
		//
		CTabHandle		GetCTabHandle(void) { return (m_ctab); }
		
		//////////
		// Load the palette into memory from the BMP file.
		//
		virtual void	_Load(void);
		
		//////////
		// Remove the palette from memory.
		//
		virtual void	_Purge(void);

		//////////
		// Get a Win32 COLORREF for the specified color.
		//
		// [in] inColor - color table index
		// [out] return - Win32 COLORREF which specifies a 32-bit RGB color
		//
		COLORREF		GetColor(int inColor);

	protected:
		//////////
		// Win32 palette handle.
		//
		HPALETTE		m_pal;
		
		//////////
		// Quicktime color table handle.
		//
		CTabHandle		m_ctab;
		
		//////////
		// QTGraphic used to get the palette and color table
		//
		QTGraphic		*m_qtg;
		
		//////////
		// Full path for the palette.
		//
		TString			m_FullPath;
		
		//////////
		// Name of the palette.
		//
		TString			m_PalName;	
		
		//////////
		// Is a native bitmap being used for the palette?
		//
		bool			m_NativeBMP;
};


/*-----------------------------------------------------------------

CLASS
    LPaletteManager

	Manager for LPalette objects.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LPaletteManager : public LResourceManager
{
public:
	//////////
	// Constructor.
	//
	LPaletteManager();

	//////////
	// Destructor.
	//
	~LPaletteManager();

	//////////
	// Initialize.  Save the current system palette and colors.
	//
	void		Init(void);
	
	//////////
	// We are coming to the front again, set the system palette use and system colors
	// to black and white.
	//
	// [in] inDC - Win32 handle to the display device context
	//
	void		Activate(HDC inDC);
	
	//////////
	// Restore the original system palette and colors.
	//
	// [in] inDC - Win32 handle to the display device context
	//
	void		Deactivate(HDC inDC);
	
	//////////
	// Same as Deactivate() except this method grabs and releases the display device context
	// using Win32 calls.
	//
	void		Reset(void);

	//////////
	// Get an LPalette by name.
	//
	// [in] inName - the name of the palette
	// [out] return - a pointer to an LPalette
	//
	LPalette	*GetPalette(TString &inName);
	
	//////////
	// Get the current palette.
	//
	// [out] return - current palette
	//
	LPalette	*GetCurrentPalette(void) { return (m_GraphPal); }

	//////////
	// Set the current palette.
	//
	// [in] inPal - LPalette to use
	// [in] inGraphPal - if true, set _graphpal variable to inPal's palette name 
	//
	void		SetPalette(LPalette *inPal, bool inGraphPal);
	
	//////////
	// Set the palette for gView to that which was specified in the last call to SetPalette().
	//
	void		ResetPalette(void);

protected:
	//////////
	// Graphics palette.
	//
	LPalette	*m_GraphPal;
	
	//////////
	// Video palette.
	//
	LPalette	*m_VideoPal;

	//////////
	// Original palette.
	//
	HPALETTE	m_OriginalPal;

	//////////
	// Do we need to reset the palette when we are finished?
	//
	bool		m_NeedResetPal;
	
	//////////
	// Have we initialized?
	//
	bool		m_NeedInit;
};

#endif // _LPalette_h_

/*
 $Log$
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

 Revision 1.3  2000/02/02 15:15:32  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
