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
// LPicture.cpp : 
//

#include "stdafx.h"
#include "LPicture.h"
#include "Globals.h"

#define DEFAULT_PICT_SIZE	307200	// assume full screen graphic 

//
//  LPicture -  Load the picture, update its size and update the
//		priority.
//
LPicture::LPicture(const char *inName) : LResource(inName)
{
	TString		theName = inName;

	m_Dib = NULL;

	m_FullPath = gConfigManager.GetGraphicsPath(theName);

	m_Dib = NULL;
	m_Qtg = NULL;

	// find out what the extension is
	TString		picName(m_FullPath);

	if (picName.Contains(".bmp", false))
		m_NativeBmp = false;// cbo_debug - try using QT for BMPs
	else
	{
		m_NativeBmp = false;

		// if no extension, assume .pic (for backwards compatibility)
		if (not picName.Contains("."))
			m_FullPath += ".pic";
	}
	
	size = DEFAULT_PICT_SIZE;
	//m_HaveSize = false;
	                                    
	Load();
}

//
//  ~LPicture - Make sure it isn't in memory. If it is, purge it.
//
LPicture::~LPicture()
{
    if (IsLoaded())
        _Purge();
}

//
//  _Load - Load the picture from disc.
// 
void LPicture::_Load()
{
	if (not IsLoaded())
	{
		// tell the manager we may need more memory
		gPictureManager.RequestMemory(size);

		if (m_NativeBmp)
			LoadDIB();
		else
			LoadQTGraphic();
	}
} 

//
//	LoadQTGraphic - Load a picture using QuickTime.
//
void LPicture::LoadQTGraphic(void)
{
	if (m_Qtg != NULL)
		return;

	m_Qtg = new QTGraphic(m_FullPath);
}


//
//	LoadDIB - The actual meat of loading the picture. Open the LPicture
//		file and turn it into a DIB.
//
void LPicture::LoadDIB(void)
{
	if (m_Dib != NULL)
		return;			// already have it

	// use Petzold code
	HDIB	hDib;

	hDib = DibFileLoad(m_FullPath.GetString());
	if (hDib == NULL)
	{
		gLog.Error("Error reading BMP file <%s>", m_FullPath.GetString());
		gMissingMediaLog.Log("%s", m_FullPath.GetString());
		return;
	}

	m_Dib = new Dib(hDib);
	if (m_Dib == NULL)
		free(hDib);
	else
		gDebugLog.Log("Load BMP file <%s>", m_FullPath.GetString());
}

//
//  _Purge - Clear the picture from memory.
//
void LPicture::_Purge()
{
    ASSERT(IsLoaded());
    	
    if (m_Dib != NULL)
    {
    	delete m_Dib;
    	m_Dib = NULL;
    }

	if (m_Qtg != NULL)
	{
		delete m_Qtg;
		m_Qtg = NULL;
	}
    	
    state = resUnloaded;
}

//
//  GetHilitePicture - Find the hilite picture, if it exists.
//
LPicture *LPicture::GetHilitePicture()
{
    LPicture 	*temp;
	TString		theName;
	int			strLen;
	const char	*strPtr;
   
	strPtr = m_Key.GetString();
	strLen = m_Key.Length();

	// See if the name already has an extension. If it does
	//		add the 'H' before the dot. If it doesn't, add
	//		the 'H' on to the end.
	if (m_Key.Contains("."))
	{
		for (int i = 0; i < strLen; i++)
		{
			if (strPtr[i] == '.')
			{
				theName += 'H';
			}
			theName += strPtr[i];
		}
	}
	else
	{
		theName = m_Key;
		theName += "H";
	}
 
    temp = gPictureManager.GetPicture(theName); 
    
    return (temp);
}

//
//	Draw - Draw the picture. We use 1 as the transparent color.
//
void LPicture::Draw(TPoint &inPt, bool inMatte /* = false */)
{
    bool	trans = false;
    POINT	drawPt;
    
	m_Origin = inPt;
    
	drawPt = inPt.GetPOINT();
 
//	gDebugLog.Log("Draw a Picture <%s>", m_Key.GetString());
//	DWORD	start_time;
//	DWORD	end_time;
//	
//	start_time = ::timeGetTime();

    if (inMatte)
    	trans = true;

	if (IsUnloaded())
		_Load();

	if (m_NativeBmp)
	{   
		if (m_Dib != NULL)
		{
    		m_Width = m_Dib->Width();
    		m_Height = m_Dib->Height();
    		
    		gView->CopyDIB(m_Dib, &drawPt, 255, trans);
		}
	}
	else
	{
		if (m_Qtg != NULL)
		{
			m_Width = m_Qtg->Width();
			m_Height = m_Qtg->Height();

			gView->DrawQTGraphic(m_Qtg, &drawPt, trans);
		}
	}

//	end_time = ::timeGetTime();
//	gDebugLog.Log("Draw took <%ld> milliseconds", end_time - start_time);
}

//
//	Draw - Draw the picture in the given rectangle.
//
void LPicture::Draw(TRect &inRect)
{
	RECT	drawRect;
	
	m_Origin.SetX(inRect.Left());
	m_Origin.SetY(inRect.Top());

	drawRect = inRect.GetRECT();

//	gDebugLog.Log("Draw a Picture <%s>", key.GetString());
//	DWORD	start_time;
//	DWORD	end_time;
//	
//	start_time = ::timeGetTime();

	if (IsUnloaded())
		_Load();

	if (m_NativeBmp)
	{ 		
		if (m_Dib != NULL)
		{ 
			m_Width = m_Dib->Width();
			m_Height = m_Dib->Height();
			
			gView->CopyDIB(m_Dib, &drawRect);
		}
	}
	else
	{
		if (m_Qtg != NULL)
			gView->DrawQTGraphic(m_Qtg, &drawRect);
	}

//	end_time = ::timeGetTime();
//	gDebugLog.Log("Draw took <%ld> milliseconds", end_time - start_time);
}

//
//	Hilite - Draw the highlight picture of a graphic. Then
//		draw ourselves again.
//
void LPicture::Hilite(TPoint &inPt, bool inMatte /* = true */)
{
    LPicture *Highlight = GetHilitePicture();

    Highlight->Draw(inPt, inMatte); 
    gView->Draw();
    
    Draw(inPt, inMatte);
    gView->Draw();
}

//
//	GetRect - Gets the current rectangle (bounds) of picture.
//
TRect LPicture::GetBounds()  
{
	TRect	bounds;

	bounds.Set(m_Origin.Y(), m_Origin.X(), 
		m_Height + m_Origin.Y(),
		m_Width + m_Origin.X());

	return (bounds);
}

//
//	GetOrigin - Return the origin of the picture.
//
TPoint LPicture::GetOrigin()  
{
    return (m_Origin);
}

//
//	GetPalette - Retrieve the palette from our dib.
//
HPALETTE LPicture::GetPalette(void)
{
	HPALETTE	retPal = NULL;

	if (m_NativeBmp)
	{
		if (m_Dib != NULL)
			retPal = m_Dib->GetPalette();
	}
	else
	{
		if (m_Qtg != NULL)
			retPal = m_Qtg->GetPalette();
	}

	return (retPal);
}

CTabHandle LPicture::GetColorTable(void)
{
	CTabHandle	retCTab = NULL;

	if ((not m_NativeBmp) and (m_Qtg != NULL))
		retCTab = m_Qtg->GetColorTable();

	return (retCTab);
}

//
//	GetPicture - 
//
LPicture *LPictureManager::GetPicture(TString &inName)
{
    LPicture     *thePict = NULL;

    thePict = (LPicture *) GetResource(inName);
  
    if (thePict == NULL) 
    {
		thePict = new LPicture(inName);
		
		if (thePict != NULL)
			AddNode(thePict);
	} 
	else
		thePict->Load();

    return (thePict);
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
