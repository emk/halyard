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
// Input.cpp : 
//

#include "stdafx.h"
#include "Input.h"
#include "Globals.h"
#include "Header.h"

//
//	InputManager
//
InputManager::InputManager()
{
	Reset();
}

void InputManager::Reset(void)
{
	memset(m_Buffer, 0, sizeof(m_Buffer));
	m_Variable.Empty();
	m_Style.Empty();
	m_Font = NULL;
	m_Bounds.Set(0, 0, 0, 0);
	m_NextCharPos = 0;
	m_CurChar = 0;
	m_MaxLength = 0;
	m_UnderScoreWidth = 0;
	m_InInput = false;
	m_ForeColor = RGB(0, 0, 0);			// white
	m_BackColor = RGB(0xFF, 0xFF, 0xFF);	// black	
}

//
//	Start
//
void InputManager::Start(TString &inVariable, TString &inStyle, 
							  TString &inMask, TRect &inBounds)
{
	Header		*theHeader;
	SIZE		usSize;
	RECT		theRect;
	int32		charHeight;
  
	Reset();

	m_Style = inStyle;			// save for text command later
 
	// before setting up the DC, get the font and color information
    theHeader = gHeaderManager.Find(inStyle.GetString());
    if (theHeader != NULL)
    {
		GraphicsTools::Color foreColor, backColor;

    	m_Font = theHeader->GetFont();
    	foreColor = theHeader->GetColor();
    	backColor = theHeader->GetHiColor();

		m_ForeColor = gView->GetColor(foreColor);
		m_BackColor = gView->GetColor(backColor);	
    }
    
    if (m_Font == NULL)
    	m_Font = gFontManager.GetDefaultFont();

	// set up the drawing environment
	SetupDC();
    ::SetFocus(hwndApp);
 
	// set the maximum length of the string
	m_MaxLength = inMask.Length();	
	if (m_MaxLength > (INPUT_BUFFER_SIZE - 1))
		m_MaxLength = INPUT_BUFFER_SIZE - 1;
    
    // get the width of an underscore
	if (::GetTextExtentPoint32(m_DC, "_", 1, &usSize))
	{
		m_UnderScoreWidth = usSize.cx;
		charHeight = usSize.cy;
	}

	// set the starting character position
    m_NextCharPos = inBounds.Left();
    
    // should get size of rect from number of chars and size of chars
	m_Bounds = inBounds;
    m_Bounds.SetBottom(inBounds.Top() + (int16) charHeight);

 	gDebugLog.Log("inputRect: T <%d>, L <%d>, B <%d>, R <%d>", 
 		m_Bounds.Top(), m_Bounds.Left(), m_Bounds.Bottom(), m_Bounds.Right());
    
    m_Variable = inVariable;

	theRect = m_Bounds.GetRECT();
 
   	// erase the whole rectangle
 	::FillRect(m_DC, &theRect, m_BackBrush);
 	
 	DrawUnderscore();
	    
    m_InInput = true;   
    
	TInterpreter::GetInstance()->Pause();
}

//
//	KeyDown - Process a key.
//
void InputManager::KeyDown(char inKey)
{
	RECT		theRect;

	if (InInput())
	{
		SetupDC();

		if (inKey == VK_BACK)
		{
			if (m_CurChar > 0)
			{
				// take one character out of the buffer
				m_CurChar--;
				m_Buffer[m_CurChar] = '\0';

				// erase both the underscore and the last character

				// fill in most of the rect to erase
				theRect.top = m_Bounds.Top();
				theRect.bottom = m_Bounds.Bottom();
				theRect.right = m_NextCharPos + m_UnderScoreWidth;

				// figure out a new character position
				NewCharPosition();

				// now set the left point of the rect
				theRect.left = m_NextCharPos;

				// fill in the rect
				::FillRect(m_DC, &theRect, m_BackBrush);

				DrawUnderscore();
			}
		}
		else if (((inKey == VK_RETURN) and (m_CurChar > 0))
			or ((inKey == VK_TAB) and (m_CurChar > 0)))
		{
			// finish the input and draw the full string
			m_Buffer[m_CurChar] = '\0';

			EraseUnderscore();

			// done with input
			gVariableManager.SetString(m_Variable.GetString(), m_Buffer);
			TInterpreter::GetInstance()->WakeUp();
			gView->DirtyRect(&m_Bounds);
			gHeaderManager.DoText(m_Style.GetString(), m_Bounds, m_Buffer, 0);
			gView->Draw();

			Reset();

			// do not put an underscore up there
		}
		else if (IsLegalInputChar(inKey))
		//else if ((__iscsym(inKey)) or (inKey == ' '))
		{
			if (m_CurChar < m_MaxLength)
			{
				// add a character to our buffer
				m_Buffer[m_CurChar] = inKey;
				m_CurChar++;

				EraseUnderscore();
				DrawChar(inKey);
				NewCharPosition();
				DrawUnderscore();
			}
		}
		else
		{
			// reject the character, put up an underscore
			DrawUnderscore();
		}

		ResetDC();
	}
}

void InputManager::SetupDC(void)
{
	HPALETTE	hPal;

	m_DC = ::GetDC(hwndApp);
	m_OldFont = (HFONT) ::SelectObject(m_DC, m_Font->GetFontHandle());
	hPal = gView->GetPalette()->GetPalHandle();
	m_OldPal = ::SelectPalette(m_DC, hPal, false);
	::RealizePalette(m_DC);						
	m_BackBrush = ::CreateSolidBrush(m_BackColor);
	::SetTextColor(m_DC, m_ForeColor);
	::SetBkColor(m_DC, m_BackColor);
}

void InputManager::ResetDC(void)
{
	::SelectObject(m_DC, m_OldFont);
	::DeleteObject(m_BackBrush);
	::SelectPalette(m_DC, m_OldPal, false);
	::ReleaseDC(hwndApp, m_DC); 
}

void InputManager::EraseUnderscore(void)
{
	RECT	theRect;

	// erase the underscore
	theRect.top = m_Bounds.Top();
	theRect.bottom = m_Bounds.Bottom();
	theRect.left = m_NextCharPos;
	theRect.right = m_NextCharPos + m_UnderScoreWidth;

	::FillRect(m_DC, &theRect, m_BackBrush);
}

void InputManager::DrawUnderscore(void)
{
	DrawChar('_');
}

void InputManager::DrawChar(char inChar)
{
	TString		str;

	::SetBkMode(m_DC, TRANSPARENT);

	str += inChar;
	::TextOut(m_DC, m_NextCharPos, m_Bounds.Top(), str.GetString(), 1);

	::SetBkMode(m_DC, OPAQUE);
}

void InputManager::NewCharPosition(void)
{
	SIZE			strSize;
	int32			strWidth = 0;

	if (::GetTextExtentPoint32(m_DC, m_Buffer, strlen(m_Buffer), &strSize))
		strWidth = strSize.cx;

	m_NextCharPos = m_Bounds.Left() + strWidth;
}

bool InputManager::IsLegalInputChar(char inKey)
{
	if ((__iscsym(inKey)) 
		or (inKey == ' ')
		or (inKey == ':')
		or (inKey == '.')
		or (inKey == '-')
		or (inKey == '+')
		or (inKey == '|')
		or (inKey == '{')
		or (inKey == '}')
		or (inKey == '(')
		or (inKey == ')')
		or (inKey == '?')
		or (inKey == '@')
		or (inKey == '#')
		or (inKey == '$')
		or (inKey == '%')
		or (inKey == '^')
		or (inKey == '&')
		or (inKey == '*')
		or (inKey == '~')
		or (inKey == '<')
		or (inKey == '>')
		or (inKey == '/')
		or (inKey == '=')
		or (inKey == ',')
		or (inKey == '\\'))
	{
		return (true);
	}
	return (false);
}

/*
 $Log$
 Revision 1.5  2002/10/08 21:42:25  emk
 Palette removal, part 1:

   * All primitives which used to take palette indices now take RGB colors.
   * Old 5L: Added DEFPALETTE command for declaring palettes without
     BMP files.  This provides backwards compatibility for old code.

 I haven't removed the palette code yet, but plan to do so as soon as the
 migration is complete.

 Revision 1.4  2002/08/22 00:12:22  emk
 3.5.4 - 21 Aug 2002 - emk

 Engine:

   * Moved many source files from Common to Common/lang/old5L, and from
     Win32/FiveL to Win32/FiveL/lang/old5l, including the index system, the
     parser and stream classes, the crypto classes and the file I/O classes.
   * Broke the dependencies between Header and TIndex, in a fashion similar
     to what I did for TStyleSheet in 3.5.1.  This means we can call
     INPUT from Scheme, which more-or-less completes the Scheme primitives.
   * Made sure that header and stylesheet names were case insensitive.

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

 Revision 1.2.8.2  2002/06/05 20:42:38  emk
 3.3.4.2 - Broke Win5L dependencies on TIndex file by moving various pieces
 of code into TWin5LInterpreter.  Windows 5L now accesses the interpreter
 through a well-defined API.  Changes:

   * Removed many direct and indirect #includes of TIndex.h.
   * Added a TInterpreter method ReloadScript, which can be called by the
     higher-level ReDoScript command.
   * Checked in some files which should have been included in the 3.3.4.1
     checkin--these files contain the initial refactorings of Card and Macro
     callsites to go through the TInterpreter interface.

 Up next: Refactor various Do* methods out of Card and into a procedural
 database.

 Revision 1.2.8.1  2002/06/06 05:47:30  emk
 3.3.4.1 - Began refactoring the Win5L interpreter to live behind an
 abstract interface.

   * Strictly limited the files which include Card.h and Macro.h.
   * Added TWin5LInterpreter class.
   * Made as much code as possible use the TInterpreter interface.
   * Fixed a few miscellaneous build warnings.

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

 Revision 1.7  2000/08/08 19:03:40  chuck
 no message

 Revision 1.6  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.5  2000/02/02 15:15:32  chuck
 no message

 Revision 1.4  1999/12/16 17:17:36  chuck
 no message

 Revision 1.3  1999/11/16 13:46:32  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
