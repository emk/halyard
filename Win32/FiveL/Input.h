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
// Input.h : 
//

#if !defined (_Input_h_)
#define _Input_h_

#define INPUT_BUFFER_SIZE	256

#include "TCommon.h"
#include "TString.h"
#include "TRect.h"
#include "TPoint.h"

#include "LFont.h"

/*-----------------------------------------------------------------

CLASS
    InputManager

	Handles input from the user, including drawing an input box
	to the screen.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class InputManager : public TObject
{
public:
	//////////
	// Constructor.
	//
	InputManager();

	//////////
	// Destructor.
	//
	~InputManager() {}

	//////////
	// Start input.  Draws an input box to the screen at the specified location
	// and pauses the gCardManager to wait for input.
	//
	// [in] inVariable - variable where user input should be placed
	// [in] inStyle - header name specifying font and color info
	// [in] inMask - specifies the maximum length of the input
	// [in] inBounds - screen location for the input box
	//
	void		Start(TString &inVariable, TString &inStyle, TString &inMask, TRect &inBounds);

	//////////
	// Process a key.  A TAB or RETURN finishs input and wakes up the gCardManager.
	//
	// [in] inKey - the key that was pressed
	//
	void		KeyDown(char inKey);

	//////////
	// Are we waiting for input?
	//
	// [out] return - true if waiting for input, false otherwise
	//
	bool		InInput(void) { return (m_InInput); }

protected:
	//////////
	// Reset all our variables.
	//
	void		Reset(void);
	
	//////////
	// Erase the text input underscore.
	//
	void		EraseUnderscore(void);
	
	//////////
	// Draw an underscore.
	//
	void		DrawUnderscore(void);
	
	//////////
	// Draw a character at m_NextCharPos.
	//
	void		DrawChar(char inChar);
	
	//////////
	// Calculates m_NextCharPos for the next character.
	//
	void		NewCharPosition(void);
	
	//////////
	// Setup the device context.
	//
	void		SetupDC(void);
	
	//////////
	// Reset the device context.
	//
	void		ResetDC(void);
	
	//////////
	// Is the specified character a legal input character?
	//
	// [in] inKey - the character
	// [out] return - true if it is a legal input character, false otherwise
	//
	bool		IsLegalInputChar(char inKey);

	//////////
	// Input buffer
	//
	char		m_Buffer[INPUT_BUFFER_SIZE];
	
	//////////
	// Once finished, input string is copied here.
	//
	TString		m_Variable;	
	
	//////////
	// Style of the input text.
	//
	TString		m_Style;
	
	//////////
	// Input text font.
	//
	LFont		*m_Font;

	//////////
	// Input rectangle.
	//
	TRect		m_Bounds;	
	
	//////////
	// X-Coordinate for the next character
	//
	int32		m_NextCharPos;	
	
	//////////
	// X-Coordinate of the current character
	//
	int32		m_CurChar;	
	
	//////////
	// Maximum length of the input string
	//
	int32		m_MaxLength;	
	
	//////////
	// Width of an underscore.
	//
	int32		m_UnderScoreWidth;

	//////////
	// Ready for input?
	//
	bool		m_InInput;		
	
	//////////
	// Foreground color for input box.
	//
	COLORREF	m_ForeColor;	
	
	//////////
	// Background color for input box.
	//
	COLORREF	m_BackColor;	

	//////////
	// Handle for device context.
	//
	HDC			m_DC;
	
	//////////
	// Save the previous font so it can be restored.
	//
	HFONT		m_OldFont;
	
	//////////
	// A solid brush with the background color (used for erasing).
	//
	HBRUSH		m_BackBrush;

};

#endif // _Input_h_

/*
 $Log$
 Revision 1.2  2002/10/09 18:38:42  emk
 3.5.7 - 9 Oct 2002 - emk

 Engines built from this code will require script changes.

   * Scheme: Changed 'for-each-item' to 'foreach', and added 'for'.
   * Added extract-docs.pl, which generates HTML manuals.
   * Added many new test cases for the new 5L language.
   * Fixed minor bugs in CryptStream*.*, as discovered by valgrind.
   * All primitives which used to take palette indices now take RGB colors.
   * Old 5L: Added DEFPALETTE command for declaring palettes without
     BMP files.  This provides backwards compatibility for old code.
   * Removed Windows cursor-clipping code because it was occassionally
     immobilizing the cursor completely.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/08/08 19:03:40  chuck
 no message

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
