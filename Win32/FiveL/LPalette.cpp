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
// LPalette.cpp : 
//

#include "stdafx.h"
#include "LPalette.h"
#include "Globals.h"
#include "Dib.h"
#include "Palettes.h"

#define DEFAULT_PAL_SIZE	1024
#define NumSysColors (sizeof(SysPalIndex)/sizeof(SysPalIndex[1]))
#define rgbBlack RGB(0,0,0)
#define rgbWhite RGB(255,255,255)

static int SysPalIndex[] = 
{
  COLOR_ACTIVEBORDER,
  COLOR_ACTIVECAPTION,
  COLOR_APPWORKSPACE,
  COLOR_BACKGROUND,
  COLOR_BTNFACE,
  COLOR_BTNSHADOW,
  COLOR_BTNTEXT,
  COLOR_CAPTIONTEXT,
  COLOR_GRAYTEXT,
  COLOR_HIGHLIGHT,
  COLOR_HIGHLIGHTTEXT,
  COLOR_INACTIVEBORDER,
  COLOR_INACTIVECAPTION,
  COLOR_MENU,
  COLOR_MENUTEXT,
  COLOR_SCROLLBAR,
  COLOR_WINDOW,
  COLOR_WINDOWFRAME,
  COLOR_WINDOWTEXT
};

static COLORREF MonoColors[] = 
{
  rgbBlack,
  rgbWhite,
  rgbWhite,
  rgbWhite,
  rgbWhite,
  rgbBlack,
  rgbBlack,
  rgbBlack,
  rgbBlack,
  rgbBlack,
  rgbWhite,
  rgbWhite,
  rgbWhite,
  rgbWhite,
  rgbBlack,
  rgbWhite,
  rgbWhite,
  rgbBlack,
  rgbBlack
};

static COLORREF OriginalColors[NumSysColors];

//
//	LPalette 
//
LPalette::LPalette(const char *name) : LResource(name)
{
	m_PalName = name;

	// cbo - note:
	//	To get 8-bit color working we will have to use QTML to get the
	//		ColorTable from graphics. We need a color table in order 
	//		to set the color info for the GWorld
	//	If we aren't using 8-bit, we can use native BMPs as they are
	//		probably faster and are certainly simpler.
	//
	FileSystem::Path path =
		FileSystem::GetPalettesDirectory().AddComponent(m_PalName.GetString());
	m_FullPath = path.ToNativePathString().c_str();

	m_NativeBMP = false;
	if (not m_PalName.Contains("."))
	{
	//	m_FullPath += ".pic";
		m_FullPath += ".bmp";
		m_NativeBMP = true;	
	}
	else if (m_PalName.Contains(".bmp", false))
		m_NativeBMP = true;
	 
	m_pal = NULL; 
	m_ctab = NULL;
	m_qtg = NULL;
	
	size = DEFAULT_PAL_SIZE;
		
    Load();
}

LPalette::~LPalette()
{
	if (IsLoaded())
		_Purge();
}

//
//	_Load - Read the palette in from the BMP file.
//
void LPalette::_Load(void)
{
	HDIB	hDib;
	Dib		*theDib = NULL;

	if (IsUnloaded())
	{
		m_pal = NULL;
		m_ctab = NULL;
		m_qtg = NULL;

		if (m_NativeBMP)
		{
			hDib = DibFileLoad(m_FullPath.GetString());
			if (hDib == NULL)
			{
				gLog.Error("Error reading BMP file <%s>", m_FullPath.GetString());
				gMissingMediaLog.Log("%s", m_FullPath.GetString());
				return;
			}

			theDib = new Dib(hDib);
			if (theDib == NULL)
			{
				gLog.Error("Error getting Palette from BMP file <%s>", m_FullPath.GetString());
				::DibDelete(hDib);		
			}
			else
			{
				gDebugLog.Log("Load palette from <%s>", m_FullPath.GetString());
				m_pal = theDib->GetPalette();

				delete theDib;
				theDib = NULL;
			}
		}
		else
		{
			// use QTGraphic to get the palette and color table
			m_qtg = new QTGraphic(m_FullPath);

			if (m_qtg == NULL)
			{
				gLog.Log("Could not open %s to get palette",
					m_PalName.GetString());

				_Purge();
				return;
			}

			m_ctab = m_qtg->GetColorTable();
			if (m_ctab == NULL)
			{
				gLog.Log("Could not get color table out of %s",
					m_PalName.GetString());

				_Purge();
				return;
			}

			m_pal = m_qtg->GetPalette(m_ctab);
			if (m_pal == NULL)
			{
				gLog.Log("Could not get palette out of %s",
					m_PalName.GetString());
				
				_Purge();
				return;
			}
		}
	}
}

//
//	_Purge - Toss the palette.
//
void LPalette::_Purge(void)
{
	if (m_pal != NULL)
	{
		::DeleteObject(m_pal);
		m_pal = NULL;
	}

	if (m_ctab != NULL)
	{
		::DisposeCTable(m_ctab);
		m_ctab = NULL;
	}

	if (m_qtg != NULL)
	{
		delete m_qtg;
		m_qtg = NULL;
	}
	
	state = resUnloaded;
}

COLORREF LPalette::GetColor(int inColor)
{
	COLORREF		theRef = RGB(0, 0, 0);
	PALETTEENTRY	thePE;
	UINT			numColors;

	if (m_ctab != NULL)
	{
		BYTE	red;
		BYTE	blue;
		BYTE	green;

		red = (**m_ctab).ctTable[inColor].rgb.red >> 8;
		blue = (**m_ctab).ctTable[inColor].rgb.blue >> 8;
		green = (**m_ctab).ctTable[inColor].rgb.green >> 8;

		theRef = RGB(red, green, blue);
	}
	else if (m_pal != NULL)
	{
		numColors = ::GetPaletteEntries(m_pal, inColor, 1, &thePE);
	
		if (numColors == 1)
			theRef = RGB(thePE.peRed, thePE.peGreen, thePE.peBlue);
	}

	return (theRef);
}

//
//	LPaletteManager
//
LPaletteManager::LPaletteManager()
{
	m_GraphPal = NULL;
	m_VideoPal = NULL;
	m_OriginalPal = NULL;
	m_NeedResetPal = false;
	m_NeedInit = true;
}

LPaletteManager::~LPaletteManager()
{
	if (m_OriginalPal != NULL)
		::DeleteObject(m_OriginalPal);
}

//
//	Init - initialize things, save system colors
//
void LPaletteManager::Init(void)
{
	LOGPALETTE	*sysPal;
	HDC			hDC;
	int			i;

	if (gView->BitDepth() <= 8)
	{
		if (m_NeedInit)
		{
			m_NeedInit = false;

			hDC = ::GetDC(NULL);

			sysPal = (LOGPALETTE *) malloc (sizeof (LOGPALETTE) + (256 * sizeof (PALETTEENTRY))) ;
			sysPal->palVersion    = 0x0300 ;
			sysPal->palNumEntries = 256 ;

			// save the current palette (sys colors and all)
			::GetSystemPaletteEntries(hDC, 0, 256, &(sysPal->palPalEntry[0]));

			// now make a palette out of it
			m_OriginalPal = ::CreatePalette(sysPal);
			free (sysPal);

			// save the system colors
			for (i = 0; i < NumSysColors; i++)
				OriginalColors[i] = ::GetSysColor(SysPalIndex[i]);
    
			Activate(hDC);
			
			::ReleaseDC(NULL, hDC);
		}
	}
}

//
//	Activate - We are coming to the front again, make sure to set the system
//		palette use and sys colors to black and white.
//
void LPaletteManager::Activate(HDC	inDC)
{
	if (gView->BitDepth() <= 8)
	{
		if (m_NeedInit)
			Init();

		if (not m_NeedResetPal)
		{
			::SetSystemPaletteUse(inDC, SYSPAL_NOSTATIC);
    		::SetSysColors(NumSysColors, SysPalIndex, MonoColors);

			// should we realize a palette here too?

			m_NeedResetPal = true;
		}
	}
}

//
//	Deactivate - 
//
void LPaletteManager::Deactivate(HDC inDC)
{
	HPALETTE	hOldPal;

	if (gView->BitDepth() <= 8)
	{
		if (m_NeedResetPal)
		{
			// set the original palette
			::UnrealizeObject(m_OriginalPal);
			hOldPal = ::SelectPalette(inDC, m_OriginalPal, false);
			::RealizePalette(inDC);

			// set the system palette stuff back to the way it was
			::SetSystemPaletteUse(inDC, SYSPAL_STATIC);
			::SetSysColors(NumSysColors, SysPalIndex, OriginalColors);

			m_NeedResetPal = false;
		}
	}
}

void LPaletteManager::Reset(void)
{
	HDC		hDC;

	hDC = ::GetDC(NULL);
	Deactivate(hDC);
	::ReleaseDC(NULL, hDC);
}

//
//	GetPalette
//
LPalette *LPaletteManager::GetPalette(TString &inName)
{
	LPalette	*thePal = NULL;
	TString		newName;
	
	if (inName.Contains("."))
	{
		// replace whatever is on the end with .bmp
		int32	charPos = inName.Find(".");

		newName = inName.Mid(0, charPos);
	}
	else
		newName = inName;

	newName += ".bmp";

	thePal = (LPalette *) GetResource(newName);	
	if (thePal == NULL)
	{
		thePal = new LPalette(newName);	
		if (thePal != NULL)
			AddNode(thePal);
	}
	else
		thePal->Load();
		
	return (thePal);
}

//
//	SetPalette
//
void LPaletteManager::SetPalette(LPalette *inPal, bool inGraphPal)
{
	if (inPal == NULL)
		return;

	if (inGraphPal)
	{
		m_GraphPal = inPal;

		gVariableManager.SetString("_graphpal", inPal->GetName());
		gView->SetPalette(inPal);

		gDebugLog.Log("Set: _graphpal to <%s>", inPal->GetName());
	}
	else
	{
		m_VideoPal = inPal;

		if (gView->BitDepth() == 8)
		{

		// set the screen palette (normally for video)
//		HDC			hDC;
//		HPALETTE	hOldPal;
//
//		hDC = ::GetDC(hwndApp);
//		::UnrealizeObject(inPal->GetPalHandle());
//		hOldPal = ::SelectPalette(hDC, inPal->GetPalHandle(), false);
//
//		::RealizePalette(hDC);
//		::ReleaseDC(hwndApp, hDC);

		// use QuickTime to realize the palette (the only way
		//	movies will draw correctly in 8-bit
			{
				WindowPtr		wPtr;
				PaletteHandle	hPalette;
				CTabHandle		cTabHand;

				cTabHand = inPal->GetCTabHandle();

				if (cTabHand != NULL)
				{
					wPtr = ::GetNativeWindowPort(hwndApp);
					hPalette = ::NewPalette((**cTabHand).ctSize+1, cTabHand, pmTolerant, 0);

					// Hook the palette to the window
					::NSetPalette(wPtr, hPalette, (short) pmAllUpdates);
					::ActivatePalette(wPtr);
				}
			}
		}

		gDebugLog.Log("Set: video palette to <%s>", inPal->GetName());
	}
}

//
//	ResetPalette
//
void LPaletteManager::ResetPalette(void)
{
	if (m_GraphPal != NULL)
		gView->SetPalette(m_GraphPal);
}

/*
 $Log$
 Revision 1.4  2002/07/25 22:25:36  emk
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

 Revision 1.3  2002/06/20 16:32:55  emk
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

 Revision 1.2.8.1  2002/06/05 08:50:52  emk
 A small detour - Moved responsibility for script, palette and data directories
 from Config.{h,cpp} to FileSystem.{h,cpp}.

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

 Revision 1.6  2000/08/08 19:03:40  chuck
 no message

 Revision 1.5  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.4  2000/03/01 15:46:55  chuck
 no message

 Revision 1.3  2000/02/02 15:15:32  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
