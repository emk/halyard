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

//////////////////////////////////////////////////////////////////////////////
//
// LFont.cpp : 
//

#include "stdafx.h"
#include "LFont.h"
#include "Globals.h"

#define DEFAULT_FONT_SIZE	2048
#define DEFAULT_FONT_NAME "_DefaultFont"

//
//	LFont - Construct a font given its height. We always use 
//		Times New Roman.
//
LFont::LFont(const char *inName, int16 inHeight) : LResource(inName)
{
	m_Height = inHeight;
	m_Bold = false;
	m_Color = 255;			// default to black

	size = DEFAULT_FONT_SIZE;
	m_FamilyName = "Times New Roman";
	m_Font = NULL;

	Load();
}

//
//	LFont - Construct a font from the name.
//
LFont::LFont(const char *name) : LResource(name)
{
    char 	buf[3];
    char 	namebuf[33];
	char	familyName[33];
    strcpy(namebuf, name);
	int 	namelen = 0;

	m_Font = NULL;
	m_Bold = false;
	m_Color = 255;

    //first, get rid of underscores:
    char	*undloc = strstr(namebuf, "_");
    for (; undloc; undloc = strstr(namebuf, "_"))
    	*undloc = ' ';

    //get size of font:
    
    namelen = strlen(namebuf);
    size = DEFAULT_FONT_SIZE;
    
    buf[0] = namebuf[namelen-2]; 
	buf[1] = namebuf[namelen-1]; 
	buf[2] = '\0'; 
    m_Height = atoi(buf); 
    
    //get rid of size info and trailing space:
   
	strncpy(familyName, namebuf, namelen - 2);
	familyName[namelen-2] = '\0';
	
	if (familyName[namelen-3] == ' ') 
		familyName[namelen-3] = '\0';

	// check for "boldness". Only valid for ser and sans default fonts right now
	// (pbs if font name begins with b otherwise).
	if (!strncmp(familyName, "b", 1) || !strncmp(familyName, "B", 1))
		m_Bold = true;

	// font is not found. substitute:
	if (strstr(familyName, "san"))
		strcpy(familyName, "Arial");
	else if (strstr(familyName, "fixed"))
		strcpy(familyName, "Courier New");
	else
		strcpy(familyName, "Times New Roman");	

	m_FamilyName = familyName;

//	gDebugLog.Log("Created font <%s>, size <%ld>", m_FamilyName, size);

	Load();
}

//  Ignore locked bit and purge if it's loaded.
//
LFont::~LFont()
{
    if (state > resUnloaded)
        _Purge();
}

//
//  Load from library.
//
void LFont::_Load()
{
    if (IsUnloaded())
	{
		// tell the manager we may need more memory
		gFontManager.RequestMemory(size);

		if (not m_Bold)
			m_Font = ::CreateFont(-m_Height,0,0,0,FW_NORMAL,0,0,0,0,OUT_TT_PRECIS,0,PROOF_QUALITY,0,m_FamilyName.GetString());
		else
			m_Font = ::CreateFont(-m_Height,0,0,0,FW_BOLD,0,0,0,0,OUT_TT_PRECIS,0,PROOF_QUALITY,0,m_FamilyName.GetString());

		if (m_Font == NULL)
    		gLog.Log("Error creating font. Please restart Windows.");
	}
}

//
//  Clear from memory.
//
void LFont::_Purge()
{
    if (m_Font != NULL) 
    	::DeleteObject(m_Font);
    	
    m_Font = NULL;
    state = resUnloaded;
}

/***************************

    FONT DISPLAY METHODS

***************************/

//  Set the current font to this font. Make sure it's loaded.
//
void LFont::Set()
{
	Load(); 
}

//  To underline or not to underline?
//  
void LFont::SetUnderline(int fUnder)
{
	HDC		hDC;
	int		ul;
			
	if (fUnder) 
		ul = 1;
	else 
		ul = 0;
	
	hDC = gView->GetDC();
	
	::SelectObject(hDC, ::GetStockObject(ANSI_FIXED_FONT));
	
	::DeleteObject (m_Font);
	
	if (not m_Bold)
		m_Font = ::CreateFont(-m_Height, 0, 0, 0, FW_NORMAL, 
		    0, ul, 0, 0, OUT_TT_PRECIS, 0, PROOF_QUALITY, 0, m_FamilyName.GetString());
	else
		m_Font = ::CreateFont(-m_Height, 0, 0, 0, FW_BOLD, 
		    0, ul, 0, 0, OUT_TT_PRECIS, 0, PROOF_QUALITY, 0, m_FamilyName.GetString());
	            
	if (m_Font == NULL)
	{
		gLog.Log("Error creating font. Please restart Windows.");
        return;
    }
    
	::SelectObject(hDC, m_Font);
}

int16 LFont::Height()
{
	if (not m_Bold)
	{
		return (m_Height);
	}
	else
	{
    	return (m_Height + 0);
    }
}             
 
int16 LFont::CharWidth(char ch)
{
    HDC     hDC;
    HFONT   hOldFont;
    int16	hCharWidth = 0;
    
    hDC = gView->GetDC();
    hOldFont = (HFONT) ::SelectObject(hDC, m_Font); 

	SIZE	charWidth;

	if (::GetTextExtentPoint32(hDC, &ch, 1, &charWidth))
		hCharWidth = (int16) charWidth.cx;

    ::SelectObject(hDC, hOldFont);
    
    return (hCharWidth);  
}

int16 LFont::CharHeight(char ch)
{
    HDC     hDC;
    HFONT   hOldFont;
    int16	hCharHeight = 0;
    
    hDC = gView->GetDC();
    hOldFont = (HFONT) ::SelectObject(hDC, m_Font); 
	
	SIZE		charWidth;

	if (::GetTextExtentPoint32(hDC, &ch, 1, &charWidth))
		hCharHeight = (int16) charWidth.cy;

/* WIN16
	
	DWORD		hCharSize;

	hCharSize = ::GetTextExtent(hDC, &ch, 1); //works better for extended ascii chars
	hCharHeight = HIWORD(hCharSize);
*/
    ::SelectObject(hDC, hOldFont);
       
    return (hCharHeight);  
}
                 

void LFont::SetColor(int32 inColor)
{
    m_Color = inColor;
}


void LFont::Draw(TPoint &pt, char ch)
{ 
	HDC			hDC;
	COLORREF	theColor;
    char    	str[2];
    
    str[0] = ch;
    str[1] = '\0';
    
    hDC = gView->GetDC();
    theColor = gView->GetColor(m_Color);
    
	//left here so shadows work:
	::SetTextColor(hDC, theColor);
    
	::TabbedTextOut(hDC, pt.X(), pt.Y(), str, strlen(str), 0, NULL, 0);
}

//
//	LFontManager -
//
LFontManager::LFontManager()
{
	AddDefaultFont();
}

//
//	GetFont
//
LFont *LFontManager::GetFont(TString &inName)
{
    LFont        *theFont;

    theFont = (LFont *) GetResource(inName);
    if (theFont == NULL) 
    {
        theFont = new LFont(inName);
        if (theFont != NULL)
        	AddNode(theFont);
    } 
    else
    	theFont->Load();
    	
    return (theFont);
}

//
//	GetDefaultFont
//
LFont *LFontManager::GetDefaultFont(void)
{
	LFont		*theFont;
	TString		defaultName(DEFAULT_FONT_NAME);

	theFont = (LFont *) GetResource(defaultName);
	
	if (theFont != NULL)
		theFont->Load();
	else
		gLog.FatalError("Can't find default font.  Quitting to avoid crash.");

	return (theFont);
}

void LFontManager::RemoveAllButDefaultFont()
{
	RemoveAll();
	AddDefaultFont();
}

void LFontManager::AddDefaultFont()
{
	// create our "default" font
	LFont		*theFont;

	theFont = new LFont(DEFAULT_FONT_NAME, 24);
	if (theFont != NULL)
		AddNode(theFont);
}

/*
 $Log$
 Revision 1.2.8.1  2002/06/11 18:03:59  emk
 Fixed a bug where 5L deleted the default font when switching scripts,
 causing INPUT to crash when passed a non-existant header name.

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

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  2000/03/01 15:46:55  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
