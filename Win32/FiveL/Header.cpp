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
// Header.cpp : 
//

#include "stdafx.h"
#include "TCommon.h"
#include "LUtil.h"
#include "Header.h"
#include "Globals.h"

static const char *INCR_Y_NAME = "_incr_y";
static const char *INCR_X_NAME = "_incr_x";

//  Let Index ancestor construct based on these values.
//
Header::Header(IndexFile *inFile, const char *name, long start, long end) :
    Index(inFile, name, start, end)
{
    itsAlign = AlignLeft;
    itsColor = itsHighlightColor = itsShadowColor = itsShadHighColor = 0;
    itsShadow = 0;
    itsOffset = 0;
}

/***********************************************************************
 * Function: Header::ParseScript
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *      Once we get the header data read into memory, parse it and fill
 *  the header fields. Then we can ditch the original data. Colors are
 * checked to match InfoWindows standards (EGA + MIC restrictions...)
 ***********************************************************************/
void Header::ParseScript()
{
    TString     align, fontname;

    // (HEADER HNAME FONTNAME...
    //
    m_Script >> open >> discard >> discard >> fontname;
    
    fontname.MakeLower();
    
    itsFont = gFontManager.GetFont(fontname); //creates a resource if dne, and loads font
    //  ...ALIGNMENT COLOR HIGHCOLOR...
    m_Script >> align >> itsColor >> itsHighlightColor; 

    align.MakeLower();
	if (align.Equal("center"))
        itsAlign = AlignCenter;
    else if (align.Equal("right"))
        itsAlign = AlignRight;
    else
        itsAlign = AlignLeft;

    //  ...SHADOW SHADCOLOR)
    if (m_Script.more()) 
    {
        m_Script >> itsShadow >> itsShadowColor;
    }
    
    //@@@9-19-96 to handle font differences bet. Win. & Mac.
    if (m_Script.more()) 
    {
        m_Script >> itsOffset;
    }
    
    if (m_Script.more()) 
    {
        m_Script >> itsShadHighColor;
    }
    
    m_Script >> close;
    FlushScript();
}

/***********************************************************************
 * Function: Header::Prep
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *      Prepare header for text printing
 ***********************************************************************/
void Header::Prep()
{
    itsFont->Set();
    itsFont->SetColor(itsColor);
}

/***********************************************************************
 * Function: HeightHeader
 *
 *  Parameter (null)
 * Return:
 *  Returns text font height (protected in the TXHEADER Font structure.)
 * Comments:
 *
 ***********************************************************************/
int Header::HeightHeader()
{
    return itsFont->Height();
}

char    *vval = new char[10];
//for return value

/***********************************************************************
 * Function: Draw
 *
 *  Parameter bounds    (where to stick it in)
 *  Parameter text      (what to stick in)
 *  Parameter color     (what color)
 *  Parameter shadow    (what shadow displacement)
 * Return:
 *
 * Comments:
 *      Bigtime major important text drawing method...
 *  The text should be space-delimited. Special characters may
 *  be embedded.  Does on-the-fly text formatting to fit text in "bounds"
 *      Also maintains the _incr_y 5L GLOBAL to reflect the last coord.
 *  of printed text (for bullet drawing)
 ***********************************************************************/
void Header::Draw(TRect &bounds, char *text, int color, int Shadow)
{
	HDC		hDC;
	HFONT	hOldFont;
	TRect	dirty_rect;
    TPoint  loc(bounds.Left(), bounds.Top());
    int     maxWidth = bounds.Right() - bounds.Left();
    int     pixWidth;
	TPoint	text_size;
	int		incr_x;
    long    tLen = strlen(text);
    long    lineStart;
    long    index = 0;
    bool	first_line = true;		// only use offset for first line

    ASSERT(itsFont);
    Prep();
    fHilite = fUnderline = false;

	incr_x = bounds.Left();
    
    // set our dirty rectangle
	dirty_rect = bounds;
    
	//use slow one
	{
		//First, set up device context:
		hDC = gView->GetDC();
	 	   	
	    hOldFont = (HFONT) ::SelectObject(hDC, itsFont->GetFontHandle());
	    
	    ::SetBkMode(hDC, TRANSPARENT);

	    //  Loop until all the text is drawn.
	    while (index < tLen) 
	    {
	        //  How much should we draw on this line?
	        //
	        lineStart = index;
	        pixWidth = GetLineLength(text, &index, tLen, maxWidth);
	
	        //  Set starting point for drawing based on alignment.
	        //
	        switch (itsAlign) 
	        {
	            case AlignLeft:
	                loc.SetX(bounds.Left());
	                break;
	            case AlignRight:
	                loc.SetX(bounds.Right() - pixWidth);
	                break;
	            case AlignCenter:
	                loc.SetX((bounds.Left() + bounds.Right() - pixWidth) >> 1);
	                break;
	        }
	
	        //  Draw the text.
	        //
	        
	        //itsFont->SetColor(color);
	        // Color support for text command removed. Color set in header.
	        
	        text_size = DrawLine(loc, text, lineStart, index);
			incr_x = max(incr_x, text_size.X());
	
	        //  Bump the y coordinate for the next line.
	        //loc.OffsetY(itsFont->Height());
			loc.OffsetY(text_size.Y());
	        
	        //  Don't bother drawing text once we hit the bottom of the screen.
	        //
	        if (loc.Y() > 480)  
				goto end;
	    }
	    
		// reset stuff
		::SetBkMode(hDC,OPAQUE);
		::SelectObject(hDC, hOldFont);

	} // end-while

end:
    gVariableManager.SetLong(INCR_Y_NAME, loc.Y()); 
	gVariableManager.SetLong(INCR_X_NAME, incr_x);
    
    // make sure the dirty rect extends down to cover characters below the
    // text line - use g as an example (should really see if there are chars
    // that are below the line - and use the right one)
    //dirty_rect.SetBottom(loc.Y() + (itsFont->CharHeight('g') - itsFont->Height()) + 2);
	dirty_rect.SetBottom(loc.Y() + 2);
	dirty_rect.SetRight(incr_x);

    gView->DirtyRect(&dirty_rect);
}

/*  SPECIAL CHARACTERS
    \t      Tab.
    \n      Newline. Hard return.
	\w		Newline (windows only)
    \*      Literals. Examples include \(, \), \$, \;, \`, \', \\
    ^       Hilite toggle.
    _       Underline toggle.
    `,'     Single smart quotes. (ASCII 212, 213)
    ``, ''  Double smart quotes. (ASCII 210, 211)

    Smart quotes work only if we're using a Mac based font that has
    smart quotes at the proper ASCII locations.
*/

/***********************************************************************
 * Function: Header::GetLineLength
 *
 *  Parameter s         (to print)
 *  Parameter index     (current position in string -- GETS MODIFIED!)
 *  Parameter tLen      (final position)
 *  Parameter maxWidth  (determined by the box & font size)
 * Return:
 *        How many chars should be printed.
 * Comments:
 *   Determine how many characters should be drawn on the line
 *  and the pixel width of that line.
 *  ALWAYS progress at least one character into the string!
 *  This routine must progress or we may loop forever...
 *  Does some weird stuff with SmartQuotes, check out the case '`': etc..
 ***********************************************************************/
int Header::GetLineLength(char *s, long *index, long tLen, int maxWidth)
{
    long            pos = *index;
    long            wordBreak = *index;
    unsigned char   ch;
    int             pixWidth = 0;
    int             pixBreak = 0;
    int             pixLast = 0;
    int             done = (*index >= tLen);

    //  Skip any leading spaces. They should not figure into the width.
    //
    while (s[pos] == ' ')
        pos++;

    while (!done) 
	{
        ch = s[pos];
        switch (ch) 
		{

            case 0:             //  End of string.
                done = true;

            case '^':           //  Hilite char. Ignore.
            case '_':           //  Underline char. Ignore.
                pos++;  
                ch = 0;         //  Set to 0 so we don't count its width
                break;

            case '\\':          //  Special char marker.

                switch (ch = s[++pos]) 
				{
                    case 0:
					case 'w':	// newline (windows only)
                    case 'n':   //  End of string or line.

                        //  If last char was a space don't count it!
                        //
                        if (pos > 1 && s[pos - 2] == ' ')
                            pixWidth = pixBreak;

                        done = true;
                        ch = 0;
                        pos++;

                    default:
                        break;
                }
                break;

            case '\'':          //  Close quote.
                if (s[pos + 1] == ch) 
				{
                    pos++;
                    ch = 211;		// 0xD3
                } 
				else 
					ch = 213;		// 0xD5
                break;

            case '`':           //  Open quote.
                if (s[pos + 1] == ch) 
				{
                    pos++;
                    ch = 210;		// 0xD2
                } 
				else 
					ch = 212;		// 0xD4
                break;

            case ' ':           //  Space.

                pixBreak = pixWidth;
                wordBreak = pos;
				break;

            default:
                break;
        }

        //  Figure spacing if the character exists.
        //
        if (ch != 0) 
		{

            pixLast = pixWidth;
            pixWidth += itsFont->CharWidth(ch);

            if (pixWidth > maxWidth) 
			{
                //  Always return one character in narrow rect.
                if (pos == *index)
                    pos++;
                //  If we have less than one word...
                else if (wordBreak == *index)
                    pixWidth = pixLast;
                //  Use the last word break.
                else 
				{
                    pixWidth = pixBreak;
                    pos = wordBreak + 1;    // plus the space
                }
                done = true;

            } 
			else 
				pos++;
        }
    }

    *index = pos;
    return pixWidth;

    return 0;

}

/***********************************************************************
 * Function: Header::DrawLine
 *
 *  Parameter loc  (where to start)
 *  Parameter s    (what to print)
 *  Parameter a    (start at a in "s")
 *  Parameter b    (..and end in b)
 * 
 * Return:
 *		a point containing the actual width (in pixels) and
 *		height (including any descenders) of the text that was output
 *
 * Comments:
 *   Draw the number of characters given, starting at s.
 ***********************************************************************/
TPoint Header::DrawLine(TPoint &loc, const char *s, long a, long b)
{
	HDC				hDC;
	COLORREF		theColor;
    TPoint			pt(loc);
    TPoint   		shad;
    unsigned char	ch;
	int				incr_x = loc.X();
	int				incr_y = itsFont->Height();
	TPoint			ret;				// return value
    
    hDC = gView->GetDC();
 
	// See if we don't have anything special to do.
	//
	if (not (strstr(s, "^")		// no hilite
		or (strstr(s, "_"))		// no underline
		or (strstr(s, "\\"))	// no special characters
		or (strstr(s, "@"))))	// no bold
	{
	    //  Skip leading spaces. They should not be drawn.
	    //
		
	    while (s[a] == ' ')
	        a++;
	    
	    const char* tptr = &s[a];
	    
	    //find line end

        itsFont->SetUnderline(fUnderline);
	
        //  Shadow first.
        //
        if (itsShadow) 
        {
            shad.Set(pt.X() + itsShadow, pt.Y() + itsShadow);
            itsFont->SetColor(fHilite ? itsShadHighColor : itsShadowColor);
			
			theColor = gView->GetColor(itsFont->Color()); 
			
			::SetTextColor(hDC, theColor);
			::TabbedTextOut(hDC, shad.X(), shad.Y() + itsOffset, tptr, (int) (b-a-1), 0, NULL, 0);
        }
	
        //  Main text.
        //
        itsFont->SetColor(fHilite ? itsHighlightColor : itsColor);
        
        theColor = gView->GetColor(itsFont->Color());
        
		::SetTextColor(hDC, theColor);

		// set incr_x and incr_y
		SIZE	textSize;
		if (::GetTextExtentPoint32(hDC, tptr, (int) (b-a-1), &textSize))
		{
			incr_x += textSize.cx;
			incr_y = max(incr_y, textSize.cy);
		}

		::TabbedTextOut(hDC, pt.X(), pt.Y() + itsOffset, tptr, (int) (b-a-1), 0, NULL, 0);
	}
	else //slow way:
	{
	    //  Skip leading spaces. They should not be drawn.
	    //

	    pt.OffsetY(itsOffset);

	    while (s[a] == ' ') 					// skip whitespace at start
	        a++;
	
	    while (a < b) 
		{
			
	        switch (ch = s[a]) 
	        {
	            case 0:							//  End of string.
	                ret.Set(incr_x, incr_y);
					return ret;
	
	            case '^':						//  Hilite char.
	                a++;
	                fHilite = !fHilite;
	                ch = 0;						//  Set to 0 so we don't draw it.
	                
	                if (fHilite)				// just starting style text
	                {
	                	while (s[a] == ' ') 	// skip spaces
	                		a++; 
	                }
	                
	                break;
	
	            case '_':
	                a++;
	                fUnderline = !fUnderline;
	                ch = 0; 
	                
	                if (fUnderline)				// just starting style text
	                {
	                	while (s[a] == ' ')		// skip spaces
	                		a++;                              
	                }
	                
	                break;

	            case '@':
	                a++;
	                itsFont->SetBold(not itsFont->Bold());
	                ch = 0;
	                
	                if (itsFont->Bold())			// just starting style text
	                {
	                	while (s[a] == ' ') 	// skip spaces
	                		a++;
	                }
	                
	                break;
	
	            case '\\':          //  Special char marker.
	
	                switch (ch = s[++a]) 
					{
	                    case 0:
						case 'w':			// newline (windows only)
	                    case 'n':           //  End of string or line.
	                        ret.Set(incr_x, incr_y);
							return ret;
	                    case 't':
	                    	ch = 9; //tab
							break;
						case 'm':			// newline (mac only)
							ch = 0;			// ignore on Windows
							break;
	                    default:
	                        break;
	                }
	                break;
	
	/*            case '\'':          //  Close quote.
	                if (s[a + 1] == ch) {
	                    a++;
	                    ch = 146;
	                } else ch = 180;
	                break;
	            case '`':           //  Open quote.
	                if (s[a + 1] == ch) {
	                    a++;
	                    ch = 210;
	                } else ch = 212;
	                break;
	*/
	           case '\'':          //  apostrophe.
	                 ch = 146;
	                 break;
	            default:
	                break;
	        }
	
	        //  Draw if the character exists.
	        //
	        if (ch != 0) 
	        {
	            //create new font, bold/underline if necessary: 
	            itsFont->SetUnderline(fUnderline); //this sets bold, too!!!
	             
	            //  Shadow first.
	            //
	            if (itsShadow) 
	            {
	                shad.Set(pt.X() + itsShadow, pt.Y() + itsShadow);
	                itsFont->SetColor(fHilite ? itsShadHighColor : itsShadowColor);
	                itsFont->Draw(shad, ch);
	            }
	
	            //  Main text.
	            //
	            itsFont->SetColor(fHilite ? itsHighlightColor : itsColor);
	            itsFont->Draw(pt, ch);
	
	            //  Move location.
	            //
	            if (ch == 9) 
					pt.OffsetX(3*itsFont->CharWidth(ch)); //tab widths
	            else 
					pt.OffsetX(itsFont->CharWidth(ch));
	            
				incr_x = pt.X();
				incr_y = max(incr_y, itsFont->CharHeight(ch));

	            a++;
	        }
	    } // end-while
	} 

	ret.Set(incr_x, incr_y);
	return ret;
}

/*****************************

    HEADER MANAGER METHODS

*****************************/

/***********************************************************************
 * Function: HeaderManager::MakeNewIndex
 *
 *  Parameter name
 *  Parameter start         (see Index class)
 *  Parameter end
 * Return:
 *
 * Comments:
 *  Create a new Header Index
 ***********************************************************************/
void HeaderManager::MakeNewIndex(IndexFile *inFile, const char *name, long start, long end)
{
    Header  *newHeader;
    
    newHeader = new Header(inFile, name, start, end);
    if (newHeader->SetScript())
	{
		newHeader->ParseScript();

		Add(newHeader);
	}
}

/***********************************************************************
 * Function: HeaderManager::DoHeader
 *
 *  Parameter headername
 * Return:
 *
 * Comments:
 *      Set the font characteristics of the header in
 *  preparation of drawing.
 ***********************************************************************/
void HeaderManager::DoHeader(const char *headername)
{
    Header  *hdr;

    hdr = (Header *)Find(headername);
    
    if (hdr == NULL)
    {
    	gLog.Log("Couldn't find header <%s>.", headername);
    	return;
    }
    
    hdr->Prep();
}

/***********************************************************************
 * Function: HeaderManager::DoText
 *
 *  Parameter header        (5L header name, not TX's)
 *  Parameter bounds        (rectangle to print)
 *  Parameter text
 * Return:
 *
 * Comments:
 *      Wrapper to call Header::Draw()...
 ***********************************************************************/
void HeaderManager::DoText(const char *header, TRect &bounds, const char *text, int color, int shadow)
{
    Header  *hdr;
    
    hdr = (Header *)Find(header);
    
    if (hdr == NULL)
    {
    	gLog.Log("Couldn't find header <%s>.", header);
    	return;
    }
    
    hdr->Draw(bounds, (char *)text, color, shadow);
}

/***********************************************************************
 * Function: HeaderManager::Height
 *
 *  Parameter header
 * Return:
 *
 * Comments:
 *      Just because Height is protected in Font...
 ***********************************************************************/
int HeaderManager::Height(const char* header)
{
    Header  *hdr;

    hdr = (Header *)Find(header);
    
    if (hdr == NULL)
    {
    	gLog.Log("Couldn't find header <%s>.", header);
    	return(0);
    }

    hdr->Prep();
    return (hdr->HeightHeader());
}

/*
 $Log$
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

 Revision 1.8  2000/10/27 15:51:25  hyjin
 remove previous revision

 Revision 1.6  2000/04/07 17:05:15  chuck
 v 2.01 build 1

 Revision 1.5  2000/03/01 15:46:55  chuck
 no message

 Revision 1.4  1999/12/16 17:17:36  chuck
 no message

 Revision 1.3  1999/11/16 13:46:32  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
