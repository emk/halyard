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
// Header.h : Text drawing class.
//

#if !defined (_Header_h_)
#define _Header_h_

#include "TCommon.h"
#include "TIndex.h"
#include "LFont.h"

//Sets up screen text buffer.
//void TEXTLOC_init();

/*-----------------------------------------------------------------

CLASS
    Header

	Text drawing class that encapsulates a set of color and font
	properties to form a reusable header.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class Header : public TIndex 
{
	public:
		//////////
		// Constructor.
		//
		// [in] inFile - TIndexFile which has the header indexed
		// [in_optional] name - name of this Header (default NULL)
		// [in_optional] p1 - starting index (default 0)
		// [in_optional] p2 - ending index (default 0)
		//
        Header(TIndexFile *inFile, const char *name = NULL, long p1 = 0, long p2 = 0);
        
        //////////
		// Get the header font.
		//
		// [out] return - the LFont for this header
		//
		LFont		*GetFont(void) { return (itsFont); }
        
		//////////
		// Get the header color.
		//
		// [out] return - the font color for this header
		//
		int			GetColor(void) {return(itsColor);}
        
		//////////
		// Get the highlight color.
		//
		// [out] return - the highlight color for this header
		//
		int			GetHiColor(void) {return(itsHighlightColor);}
        
		//////////
		// Once we get the header data read into memory, parse it and fill the 
		// header fields. Then we can ditch the original data. Colors are
        // checked to match InfoWindows standards (EGA + MIC restrictions...)
		//
		virtual void ParseScript(void);
        
		//////////
		// Prepare header for text printing
		//
		virtual void Prep(void);
        
		//////////
		// Bigtime major important text drawing method. <br>
		// The text should be space-delimited. Special characters may
		// be embedded.  Does on-the-fly text formatting to fit text in "bounds"
		// Also maintains the _incr_y 5L GLOBAL to reflect the last coordinate
		// of printed text (for bullet drawing)
		//
		// [in] bounds - a TRect specifying where to put the text
		// [in] text - the text being displayed
		// [in] color - text color
		// [in] Shadow - text shadow displacement
		//
		virtual void Draw(TRect &bounds, char *text, int color, int Shadow);
        
		//////////
		// Get the header font height.
		//
		// [out] return - Returns text font height
		//
		int HeightHeader();
        
		//////////
		// Determine how many characters should be drawn on the line
		// and the pixel width of that line. Always progress at least 
		// one character into the string! This routine must progress 
		// or we may loop forever.<br>
		// Does some weird stuff with SmartQuotes, check out the case '`': etc..
		//
		// [in] s - string of text
		// [in/out] index - current position in string -- GETS MODIFIED!
		// [in] tLen - final position
		// [in] maxWidth - determined by the box & font size
		// [out] return - the number of chars that should be printed.
		//
		virtual int GetLineLength(const char *s, long *index, long tLen, int maxWidth);
        
		//////////
		// Draw the number of characters given, starting at s.
		//
		// [in] loc - the TPoint where to start drawing
		// [in] s, - string of text to print
		// [in] a - starting index in s
		// [in] b - ending index in s
		// [out] return - the actual width (in pixels) of the text that was output
		//
		virtual int DrawLine(TPoint &loc, const char *s, long a, long b);

	protected:
        //////////
		// Header font.
		//
		LFont        *itsFont;
        
		//////////
		// Header alignment.
		//
		Alignment   itsAlign;
        
		//////////
		// Header color.
		//
		int16       itsColor;
        
		//////////
		// Header highlight color.
		//
		int16       itsHighlightColor;
        
		//////////
		// Does this header have a shadow?
		//
		int16       itsShadow;
        
		//////////
		// Shadow color.
		//
		int16       itsShadowColor;
        
		//////////
		// Shadow highlight color.
		//
		int16		itsShadHighColor;  
        
		//////////
		// Used to handle font alignment differences between Windows and Mac versions
		//
		int16		itsOffset;

        //////////
		// Is the header font highlighted?
		//
		int16		fHilite;
		
		//////////
		// Is the header font underlined?
		//
		int16		fUnderline;
};


/*-----------------------------------------------------------------

CLASS
    HeaderManager

	Manages a set of Header objects.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class HeaderManager : public TIndexManager 
{
    public:
        //////////
		// Set the font characteristics of the header in preparation for drawing.
		//
		// [in] headername - name of the header
		//
		virtual void	DoHeader(const char *headername);
        
		//////////
		// Calls the Draw() method for the specified Header. 
		//
		// [in] header - name of the Header
		// [in] bounds - a TRect specifying where to put the text
		// [in] text - the text being displayed
		// [in] color - text color
		// [in] Shadow - text shadow displacement
		//
		virtual void	DoText(const char *header, TRect &bounds, const char *text,
							   int color, int shadow);
        
		//////////
		// Create a new Header Index
		//
		// [in] inFile - TIndexFile which has the header indexed
		// [in] name - name of the Header
		// [in] start - starting index
		// [in] end - ending index
		//
		virtual void	ProcessTopLevelForm(TIndexFile *inFile,
											const char *name, long start,
											long end);
        
		//////////
		// Get the height of the font in the specified header.
		//
		// [in] header - name of the Header
		// [out] return - the height of the font
		//
		int				Height(const char* header);
};

#endif // _Header_h_

//////////
// Global Header Manager
//
extern HeaderManager gHeaderManager;

/*
 $Log$
 Revision 1.7  2002/08/17 01:42:12  emk
 3.5.1 - 16 Aug 2002 - emk

 Added support for defining stylesheets in Scheme.  This means that Scheme
 can draw text!  (The INPUT doesn't work yet, because this relies on the
 separate, not-yet-fixed header system.)  This involved lots of refactoring.

   * Created TTopLevelFormProcessor as an abstract superclass of
     TIndexManager, and modified TParser to use TTopLevelFormProcessor.
     This allows the legacy 5L language to contain non-TIndex tlfs.
   * Implemented a TPrimitiveTlfProcessor class, which allows
     top-level-forms to be implemented as calls to regular 5L primitives.
   * Yanked our ValueOrPercent support from TStream into the
     TArgumentList superclass, and implemented it for all TArgumentList
     subclasses.  This allows non-5L languages to specify the funky
     percentage arguments used by the DEFSTYLE command.
   * Removed all TIndex/TIndexManager support from TStyleSheet, and
     reimplemented it using an STL std::map.  This breaks the dependencies
     between stylesheets and the old 5L interpreter.
   * Implemented a DEFSTYLE primitive.

 Revision 1.6  2002/06/20 16:32:54  emk
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

 Revision 1.5.2.1  2002/06/06 05:47:30  emk
 3.3.4.1 - Began refactoring the Win5L interpreter to live behind an
 abstract interface.

   * Strictly limited the files which include Card.h and Macro.h.
   * Added TWin5LInterpreter class.
   * Made as much code as possible use the TInterpreter interface.
   * Fixed a few miscellaneous build warnings.

 Revision 1.5  2002/05/29 13:58:17  emk
 3.3.4 - Fixed various crash-on-exit problems (including those in TBTree,
 TIndex and TLogger::FatalError), and reverted the Win32 _INCR_Y code
 to the behavior that shipped with Genetics.

 Revision 1.4  2002/05/15 11:05:33  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.3.2.1  2002/04/30 07:57:31  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

 Revision 1.3  2002/03/13 12:57:18  emk
 Support for 7-bit source code--smart quotes, m-dashes, ellipsis and HTML
 entities are now integrated into the Windows engine.

 Revision 1.2  2002/02/27 13:21:12  tvw
 Bug #613 - Changed calculation of _INCR_Y to include descenders
 (part or letter that goes below baseline).

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/04/07 17:05:15  chuck
 v 2.01 build 1

 Revision 1.3  1999/11/16 13:46:32  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
