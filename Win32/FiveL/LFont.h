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
// LFont.h : 
//

#if !defined (_LFont_h_)
#define _LFont_h_

#include "LResource.h"

/*-----------------------------------------------------------------

CLASS
    LFont

	A graphical font class.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LFont : public LResource 
{
    public:
		//////////
		// Construct a font given its height. Font is always Times New Roman.
		//
		LFont(const char *inName, int16 inHeight);
        
		//////////
		// Construct from the name. Font is always Times New Roman.
		//
		LFont(const char *name);
        
		//////////
		// Destructor.  Ignores the locked bit.
		//
		virtual 		~LFont();
        
		//////////
		// Load the font into memory.
		//
		virtual void   _Load(void);
        
		//////////
		// Unload the font from memory.
		//
		virtual void   _Purge(void);

		//////////
		// Get character width of the given character for this font.
		//
		// [in] ch - a character
		// [out] return - character width
		//
        int16     		CharWidth(char ch); 
        
		//////////
		// Get character height of the given character for this font.
		//
		// [in] ch - a character
		// [out] return - character height
		//
		int16			CharHeight(char ch);
        
		//////////
		// Set the current font to this font. Make sure it's loaded.
		//
		void    		Set(void);
        
		//////////
		// Set font color.
		//
		// [in] inColor - the color
		//
		void    		SetColor(int32 inColor);
        
		//////////
		// To underline or not to underline?
		//
		// [in] fUnder - if true (non-zero), the font will be underlined
		//
		void    		SetUnderline(int fUnder);
		
		//////////
		// Set bold attribute.
		//
		// [in] inBold - true, make font bold; false turn bold off
		//
		void			SetBold(bool inBold) { m_Bold = inBold; }
        
		//////////
		// Draw a character to the screen at the specified location.
		//
		// [in] pt - starting position
		// [in] ch - character
		//
		void    		Draw(TPoint &pt, char ch);

		//////////
		// Get a Win32 font handle for the font.
		//
		// [out] return - Win32 font handle
		//
		HFONT			GetFontHandle(void)
					{ return (m_Font); }
		
		//////////
		// Get the font height.
		//
		// [out] return - font height
		//
		int16			Height(void);
		
		//////////
		// Get font color.
		//
		// [out] return - font color
		//
		int32			Color(void)
					{ return (m_Color); }
		
		//////////
		// Is the font bold?
		//
		// [out] return - true if the font is bold, false otherwise
		//
		bool			Bold(void)
					{return (m_Bold); }
    
    protected:
        //////////
		// Win32 font handle.
		//
		HFONT   		m_Font;  
        
		//////////
		// Height of the font.
		//
		int16     		m_Height; 
        
		//////////
		// Font color.
		//
		int32     		m_Color;
        
		//////////
		// Is the font bold?
		//
		bool			m_Bold;
        
		//////////
		// Font family
		//
		TString			m_FamilyName;
};

/*-----------------------------------------------------------------

CLASS
    LFontManager

	Manages a set of LFont objects.  Includes a default font.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LFontManager : public LResourceManager
{
	public:
		//////////
		// Constructor.  Creates the default LFont.
		//
		LFontManager();
		
		//////////
		// Destructor.
		//
		~LFontManager() {}

		//////////
		// Get a font by name.
		//
		// [in] inName - name of the font
		// [out] return - pointer to the font, or NULL if not found
		//
		LFont		*GetFont(TString &inName);
		
		//////////
		// Get the default font.
		//
		// [out] return - pointer to the default font
		//
		LFont		*GetDefaultFont(void);

		//////////
		// Delete everything but the default font from our tree.  Call
		// this instead of RemoveAll if you want to unload all fonts.
		// XXX - This is a wart, but it fixes a bug in a mostly obsolete
		// class, so I decided to not refactor the TBTree subclasses to
		// make this well-integrated.
		// 
		void RemoveAllButDefaultFont();

	private:
		//////////
		// Add the default font object to our tree.
		//
		void AddDefaultFont();
};

// DEPRECATED 
//
//HFONT 		MakeNewFont(int fontnum);

#endif // _LFont_h_

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

 Revision 1.1.10.1  2002/06/11 18:03:59  emk
 Fixed a bug where 5L deleted the default font when switching scripts,
 causing INPUT to crash when passed a non-existant header name.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
