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
// QTGraphic.cpp : 
//

#include "stdafx.h"

#include "TCommon.h"

#include "Globals.h"
#include "QTGraphic.h"

QTGraphic::QTGraphic()
{
	Init();
}

QTGraphic::QTGraphic(TString &inPath)
{
	Init();
	Create(inPath);
}

QTGraphic::~QTGraphic()
{
	Toss();
}

void QTGraphic::Init(void)
{
	m_gi = NULL;
	m_idh = NULL;
	m_transparent = false;
	m_path = "";
}

void QTGraphic::Toss(void)
{
	if (m_idh != NULL)
	{
		::DisposeHandle((Handle) m_idh);
		m_idh = NULL;
	}

	if (m_gi != NULL)
	{
		::CloseComponent(m_gi);
		m_gi = NULL;
	}

	m_transparent = false;
	m_path = "";
}

void QTGraphic::Create(TString &inPath)
{
	FSSpec		fsSpec;
	OSErr		err;

	Toss();

	m_path = inPath;

	err = ::NativePathNameToFSSpec((char *) m_path.GetString(), &fsSpec, 0);
	if (err != noErr)
	{
		gDebugLog.Log("Could not get FSSpec for path <%s>",
			m_path.GetString());

		gMissingMediaLog.Log("%s, error %d", m_path.GetString(), err);
		return;
	}

	err = ::GetGraphicsImporterForFile(&fsSpec, &m_gi);
	if(err != noErr)
	{
		gDebugLog.Log("Could not get GraphicsImporter for graphic <%s>, error = <%ld>",
			m_path.GetString(), err);

		gMissingMediaLog.Log("%s, error %d", m_path.GetString(), err);
		return;
	}

	// Turn off color correction.  This tends to produce ugly
	// results, such as graphics which get corrected differently
	// from the background which they should match (bug #986).
	long flags = kGraphicsImporterDontDoGammaCorrection;
	if (::GraphicsImportSetFlags(m_gi, flags) != noErr)
	    gDebugLog.Log("Could not disable gamma correction for <%s>",
			  m_path.GetString());

	// should we get the info now or wait till somebody asks for it??
	ComponentResult cr;

	cr = ::GraphicsImportGetImageDescription(m_gi, &m_idh);
	if (cr != noErr)
	{
		gDebugLog.Log("Could not get image description for graphic <%s>, error = <%ld>",
			m_path.GetString(), cr);
		m_idh = NULL;
	}
}

CTabHandle QTGraphic::GetColorTable(void)
{
	CTabHandle	cTab = NULL;
	OSErr		err;

	if (HaveInfo())
	{
		// grab the color table
		err = GetImageDescriptionCTable(m_idh, &cTab);
		if (err != noErr)
		{
			gDebugLog.Log("Could not get color table out of graphic <%s>, error = <%d>",
				m_path.GetString(), err);

			cTab = NULL;
		}

		if (cTab != NULL)
		{
			::CTabChanged(cTab);	// to bump the color table seed

//			for (int i = 0; i < 256; i++)
//			{
//				gDebugLog.Log("Create: %d -> R <%d>, G <%d>, B <%d>",
//					i, (**cTab).ctTable[i].rgb.red, 
//					(**cTab).ctTable[i].rgb.green, 
//					(**cTab).ctTable[i].rgb.blue);
//			}
		}
	}

	return (cTab);
}

HPALETTE QTGraphic::GetPalette(CTabHandle inCTab /* = NULL */)
{
	CTabHandle	cTab = NULL;
	HPALETTE	retPal = NULL;
	LOGPALETTE	*plp = NULL;
	int			numEntries;
	bool		deleteWhenDone = false;

	if (inCTab == NULL)
	{
		cTab = GetColorTable();
		if (cTab == NULL)
			return (NULL);
		deleteWhenDone = true;
	}
	else
		cTab = inCTab;

	numEntries = (**cTab).ctSize + 1;
	plp = (LOGPALETTE *) malloc (sizeof (LOGPALETTE) + (numEntries * sizeof (PALETTEENTRY)));

	plp->palVersion    = 0x0300;
	plp->palNumEntries = numEntries;

	for (int i = 0 ; i < numEntries ; i++)
	{
	  plp->palPalEntry[i].peRed   = (**cTab).ctTable[i].rgb.red >> 8;
	  plp->palPalEntry[i].peGreen = (**cTab).ctTable[i].rgb.green >> 8;
	  plp->palPalEntry[i].peBlue  = (**cTab).ctTable[i].rgb.blue >> 8;
	  plp->palPalEntry[i].peFlags = 0;
	}
	retPal = ::CreatePalette (plp);
	free (plp);

	if (deleteWhenDone)
		::DisposeCTable(cTab);

	return (retPal);
}

long QTGraphic::Height(void)
{
	long	height = 0;

	if (HaveInfo())
		height = (*m_idh)->height;

	return (height);
}

long QTGraphic::Width(void)
{
	long	width = 0;

	if (HaveInfo())
		width = (*m_idh)->width;

	return (width);
}

long QTGraphic::BitCount(void)
{
	long	bitcount = 0;

	if (HaveInfo())
		bitcount = (*m_idh)->depth;

	return (bitcount);
}

bool QTGraphic::SetQTGWorld(GWorldPtr inGWorld)
{
	ComponentResult	cr;
	bool			retValue = false;

	if (HaveGraphic())
	{
		//cr = ::GraphicsImportSetGWorld(m_gi, NULL, NULL);
		cr = ::GraphicsImportSetGWorld(m_gi, (CGrafPtr) inGWorld, NULL);

		if (cr == noErr)
			retValue = true;
		else
		{
			gDebugLog.Log("Could not set GWorld of graphic <%s>, error = <%ld>",
				m_path.GetString(), cr);
		}
	}
	return (retValue);
}

bool QTGraphic::SetTransparent(void)
{
	ComponentResult cr;
	RGBColor		color;
	bool			retValue = false;

	if (HaveGraphic())
	{	
		// transparent color is pure white
		color.red = 0xFFFF;
		color.green = 0xFFFF;
		color.blue = 0xFFFF;

		cr = ::GraphicsImportSetGraphicsMode(m_gi, transparent, &color);

		if (cr == noErr)
			retValue = true;
		else
		{
			gDebugLog.Log("Could not set transparent drawing mode for graphic <%s>, error = <%ld>",
				m_path.GetString(), cr);
		}
	}
	return (retValue);
}

bool QTGraphic::SetDestRect(Rect *inRect)
{
	ComponentResult	cr;
	bool			retValue = false;

	if (HaveGraphic())
	{
		cr = ::GraphicsImportSetDestRect(m_gi, inRect);

		if (cr == noErr)
			retValue = true;
		else
		{
			gDebugLog.Log("Could not set destination rectangle for graphic <%s>, error = <%ld>",
				m_path.GetString(), cr);
		}
	}
	return (retValue);
}

bool QTGraphic::NeedClip(RECT *inRect)
{
	long	width;
	long	height;

	width = inRect->right - inRect->left;
	height = inRect->bottom - inRect->top;

	if (width < Width())
		return (true);
	if (height < Height())
		return (true);

	return (false);
}

void QTGraphic::Draw(GWorldPtr inGWorld, POINT *inPt, bool inTrans /* = false */)
{
	ComponentResult	cr;
	Rect			destRect;

	if (HaveGraphic())
	{
		CGrafPtr	origPort;
        GDHandle	origDev;

		GetGWorld(&origPort, &origDev);
		SetGWorld(inGWorld, NULL);

		if (not SetQTGWorld(inGWorld))
			return;

		if (inTrans)
		{
			if (not SetTransparent())
				return;
		}

		// Set the destination rect.
		destRect.top = (short) inPt->y;
		destRect.left = (short) inPt->x;
		destRect.right = (short) (destRect.left + Width());
		destRect.bottom = (short) (destRect.top + Height());

		if (not SetDestRect(&destRect))
			return;

		cr = ::GraphicsImportDraw(m_gi);

		if (cr != noErr)
		{
			gDebugLog.Log("Error drawing graphic <%s>, error <%ld>",
				m_path.GetString(), cr);
		}
		SetGWorld(origPort, origDev);
	}
}

void QTGraphic::Draw(GWorldPtr inGWorld, RECT *inRect)
{
	ComponentResult	cr;
	Rect			destRect;

	if (HaveGraphic())
	{
		CGrafPtr	origPort;
        GDHandle	origDev;

		GetGWorld(&origPort, &origDev);
		SetGWorld(inGWorld, NULL);

		if (not SetQTGWorld(inGWorld))
			return;

		// See if we need to adjust the clip rect.
		destRect.top = (short) 0;
		destRect.left = (short) 0;
		destRect.right = (short) (destRect.left + Width());
		destRect.bottom = (short) (destRect.top + Height());

		if (not SetDestRect(&destRect))
			return;

		if (NeedClip(inRect))
		{
			RgnHandle	rgn;

			rgn = ::NewRgn();
			if (rgn != NULL)
			{
				::MacSetRectRgn(rgn, (short) inRect->left, (short) inRect->top,
					(short) inRect->right, (short) inRect->bottom);

				cr = ::GraphicsImportSetClip(m_gi, rgn);

				if (cr != noErr)
				{
					gDebugLog.Log("Error drawing graphic <%s>, error = <%ld>",
						m_path.GetString(), cr);
				}

				::DisposeRgn(rgn);
			}
			else
				gDebugLog.Log("Could not get region handle to clip graphic <%s>",
					m_path.GetString());
		}

		cr = ::GraphicsImportDraw(m_gi);

		if (cr != noErr)
		{
			gDebugLog.Log("Error drawing graphic <%s>, error <%ld>",
				m_path.GetString(), cr);
		}
		::SetGWorld(origPort, origDev);
		::GraphicsImportSetClip(m_gi, NULL);
	}
}

/*
 $Log$
 Revision 1.3  2002/07/08 16:43:56  emk
 3.3.11 - Bugfixes from 3.2.0.5 through 3.2.0.7.

   * Ported Win32 QuickTime 6/VP3 bugfix forward from 3.2.0.x.
   * Ported Win32 QuickTime 6 gamma bugfix forward from 3.2.0.x.
   * Ported Win32 line drawing bugfix forward from 3.2.0.x.
   * Fixed Win32 (touch ...) command to highlight touchzones more like the
     Macintosh.  (It now redraws the unhighlighted graphic at the end of the
     highlight sequence.)

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

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
