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

#include "THeader.h"


#include "CMac5LApp.h"
#include "QTGraphic.h"
#include "CConfig.h"

USING_NAMESPACE_FIVEL

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
	m_size = 0;
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
	m_size = 0;
}

void QTGraphic::Create(TString &inPath)
{
	FSSpec		fsSpec;
	OSErr		err;

	Toss();

	m_path = inPath;

	if (not theConfig->FillSpec(&fsSpec, m_path))
	{
		gDebugLog.Log("Could not find graphic for path <%s>",
			m_path.GetString());
		gLog.Log("Error: Missing graphic: <%s>", m_path.GetString()); // Douglas wants 5L log to show this error 

		gMissingMediaLog.Log("%s", m_path.GetString());
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

	// should we get the info now or wait till somebody asks for it??
	ComponentResult cr;
	
	pascal ComponentResult GraphicsImportGetDataOffsetAndSize (
                     GraphicsImportComponent ci,
                     unsigned long *offset,
                     unsigned long *size);
    
    uint32		offset;                 
	cr = ::GraphicsImportGetDataOffsetAndSize(m_gi, &offset, &m_size);                   

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
//					   (**cTab).ctTable[i].rgb.green, 
//					   (**cTab).ctTable[i].rgb.blue);
//			}
		}
	}

	return (cTab);
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

bool QTGraphic::SetDestRect(TRect &inRect)
{
	ComponentResult	cr;
	Rect			dstRect = inRect.GetRect();
	bool			retValue = false;

	if (HaveGraphic())
	{
		cr = ::GraphicsImportSetDestRect(m_gi, &dstRect);

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

bool QTGraphic::NeedClip(TRect &inRect)
{
	if (inRect.Width() < Width())
		return (true);
	if (inRect.Height() < Height())
		return (true);

	return (false);
}

void QTGraphic::Draw(GWorldPtr inGWorld, TPoint &inPt, bool inTrans /* = false */)
{
	ComponentResult	cr;
	TRect			destRect;

	if (HaveGraphic())
	{
		CGrafPtr	origPort;
        GDHandle	origDev;

		if (inGWorld != NULL)
		{
			GetGWorld(&origPort, &origDev);
			SetGWorld(inGWorld, NULL);

			if (not SetQTGWorld(inGWorld))
				return;
		}

		if (inTrans)
		{
			if (not SetTransparent())
				return;
		}

		// Set the destination rect.
		destRect.SetTop(inPt.Y());
		destRect.SetLeft(inPt.X());
		destRect.SetRight(destRect.Left() + Width());
		destRect.SetBottom(destRect.Top() + Height());

		if (not SetDestRect(destRect))
			return;

		cr = ::GraphicsImportDraw(m_gi);

		if (cr != noErr)
		{
			gDebugLog.Log("Error drawing graphic <%s>, error <%ld>",
				m_path.GetString(), cr);
		}

		if (inGWorld != NULL)
			SetGWorld(origPort, origDev);
	}
}

void QTGraphic::Draw(GWorldPtr inGWorld, TPoint &inPt, TRect &inRect)
{
	ComponentResult	cr;
	TRect			destRect;

	if (HaveGraphic())
	{
		CGrafPtr	origPort;
        GDHandle	origDev;

		if (inGWorld != NULL)
		{
			GetGWorld(&origPort, &origDev);
			SetGWorld(inGWorld, NULL);

			if (not SetQTGWorld(inGWorld))
				return;
		}

		// See if we need to adjust the clip rect.
		destRect.SetTop(inPt.Y());
		destRect.SetLeft(inPt.X());
		destRect.SetRight(destRect.Left() + Width());
		destRect.SetBottom(destRect.Top() + Height());

		if (not SetDestRect(destRect))
			return;

		if (NeedClip(inRect))
		{
			RgnHandle	rgn;

			rgn = ::NewRgn();
			if (rgn != NULL)
			{
				::MacSetRectRgn(rgn, (short) inRect.Left(), (short) inRect.Top(),
					(short) inRect.Right(), (short) inRect.Bottom());

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
 Revision 1.6  2002/05/03 12:24:26  hamon
 Changed debug log message to read could not find graphic instead of could not get FSSpec.

 Changes by Elizabeth, per Douglas's request.

 Revision 1.5  2002/04/25 09:21:04  hamon
 Deleted #ifdef DEBUG lines when they surround gDebugLog.Log messages.

 Thanks to Eric's changes, determination of whether to actually log them is done within the variable manager and so #ifdef DEBUG lines no longer required.

 Changes by Elizabeth Hamon.

 Revision 1.4  2002/03/04 15:42:49  hamon
 Changed calls to KString, KRect etc to TString, TRect, etc to reflect new names of merged common code.

Added namespace support for compiler. New files that use TString or TArray need to specify the FiveL namespace to differentiate between Code Warrior's template string and array classes and FiveL's common string and array classes.

Changes by Elizabeth and Eric, okayed by Eric.

 Revision 1.3  2002/02/25 11:22:42  hamon
 Added missing graphic  error message to 5L log per Douglas's request.

Changes by Elizabeth Hamon, okayed by Eric.

 Revision 1.2  2000/06/15 13:03:07  chuck
 2.01 b4

 Revision 1.1  2000/05/11 13:00:22  chuck
 v 2.01 b1

*/
