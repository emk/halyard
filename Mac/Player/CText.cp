/* =================================================================================
	CText.cp	
	
	Top-level class for 5L text drawing. Designed to be mixed into a PowerPlant
	custom class declaration - if you create a CText object directly it has no
	display context (use LPane, LButton etc.).
	
	The approach here is to try to figure out as much as possible about drawing
	the string before we actually draw it. This means some pretty moosy preprocessing
	to figure out when to change the color, underline, etc. We can then draw each
	of the substrings in the appropriate style.
	
	Needs the header manager to be working before it's complete.
   ================================================================================= */

#include "THeader.h"

#include "TLogger.h"
#include "TRect.h"

#include "CMac5LApp.h"
#include "CHeader.h"
#include "CPlayerView.h"
#include "CText.h"
#include "TVariable.h"
#include "TCommon.h"
#include "TEncoding.h"

USING_NAMESPACE_FIVEL

// used to SetupText to process out control characters
const int kFinalStrLen = 4096;
static uint8 finalStr[kFinalStrLen];



/* ---------------------------------------------------------------------------------
		¥ CText() - Default constructor
   --------------------------------------------------------------------------------- */

CText::CText(void)
{
	mText = nil;	// Universal signal to not draw anything
	mStyleOffsets = nil;
}

/* ---------------------------------------------------------------------------------
		¥ CText(&header name, Bounds, &text)
   --------------------------------------------------------------------------------- */

// Helper function.
// TODO - Refactor back into TEncoding (somehow) once logging code is merged between
// Macintosh and Windows engines.
static void LogEncodingErrors (const std::string &inBadString, size_t inBadPos,
							   const char *inErrMsg)
{
	gLog.Caution("ENCODING WARNING: %s at position %d in string <<%s>>.",
		inErrMsg, inBadPos, inBadString.c_str());
}
	
CText::CText(
	const char		*inHeader,		// Name of header format to use.
	const TRect		&inBounds,		// Bounding rect (relative to PlayerView's rect)
	const char 		*inText)		// The 'raw' string (including format chars)
{
	CHeader			*theHeader;
	
	mText = nil;
	mStyleOffsets = nil;
	
	if (inText == nil)
		return;
	
	// Find the header in the header tree
	theHeader = (CHeader *) gHeaderManager.Find(inHeader);
	if (theHeader == NULL)
	{
		gLog.Caution("Couldn't find Header <%s>.", inHeader);
		
		return;
	}	

	// Totally experimental entity code.
	TEncoding<char> encoding("macintosh", &LogEncodingErrors);
	TString encodedStr = encoding.TransformString(inText);

	// Copy the incoming string
	mText = (uint8 *) ::NewPtr(strlen(encodedStr));
	strcpy((char *) mText, encodedStr);
	
	// Assign drawing rect
	mDrawRect = inBounds.GetRect();
	
	// Drop shadow dist. (from header)
	mShadowPix	= theHeader->GetShadow();
	
	// Base & highlight colors (from header)
	mBaseColor 	= theHeader->GetColor();
	mHiColor	= theHeader->GetHighlightColor();
	mShadColor	= theHeader->GetShadowColor();
	
	// Font & size (also from header)
	mFont		= theHeader->GetFontFamily();	
	mSize		= theHeader->GetHeight();		
	mJust		= theHeader->GetAlignment();
	mBold		= theHeader->GetBold();
	
	// Init the list of style elements
	mStyleOffsets = new PP::LArray(sizeof(sTextStyle));
	
	// Now that we've set up all the parameters, process the string to 
	// determine line breaks and formatting.
	
	try
	{
		SetupText();
	}
	catch (const PP::LException& inException) 
	{
		SignalPStr_("\pError in SetupText (string too long?)");
		
		if (mText != nil)
		{
			::DisposePtr((char *) mText);
			mText = nil;
		}
		
		if (mStyleOffsets != nil)
		{
			delete mStyleOffsets;
			mStyleOffsets = nil;
		}
	}
}

// ---------------------------------------------------------------------------------
//		¥ ~CText
// ---------------------------------------------------------------------------------

CText::~CText()
{
	// Delete string
	if (mText != nil)
		::DisposePtr((char *) mText);
		
	if (mStyleOffsets != nil)
		delete mStyleOffsets;
}

/* ---------------------------------------------------------------------------------
		¥ SetupText
		
		Parses the assigned string, and creates a list of offsets into the string
		with associated styles (highlight color, underline). It then resets the mText
		member to the string to draw (without the formatting characters).
		
		Each style entry is created AFTER we determine that the style has changed.
		We do this because we need to know the length of the text in the style.
		When we run out of text we create a final entry in the list.
   --------------------------------------------------------------------------------- */
void
CText::SetupText()
{
	uint8	ch;							// Individual char we're looking at
	int16	index = 0;					// Index into mText
	int16	len;						// length of mText
	int16	styleLen = 0;				// Length of style fragment
	int16	styleStart = 0;				// Starting index into mText of this style				
	int16	finalLen = 0;				// Length of final string (w/o formatting chars)
	int16	spaceCount = 0;				// number of spaces in a run
	Boolean	fHighlite, fUnderline;		// TRUE if we should do this
	Boolean	fBold;						// TRUE if we should do this too
	Boolean	isSpace;					// TRUE is processing a space
	Boolean	haveDoneStyle = false;		// TRUE if we have done at least one style
	
	if (mText == nil)
		return;
		
	// Init various vars
	fHighlite = fUnderline = fBold = false;
	
	// get len of existing text
	len = strlen((char *) mText);
		
    //  Skip leading spaces. They should not be drawn.
    while (mText[index] == ' ')
    	index ++;

	// Init style start index
	styleStart = index;
	
    while (index < len)
    {    	
    	isSpace = false;
        switch (ch = mText[index])
        {
            case '\0':             //  End of string. Should never get here, but...
				CreateStyleEntry(styleStart, styleLen, fHighlite, fUnderline, fBold);
				haveDoneStyle = true;
				
				finalStr[finalLen] = '\0';								// terminate finalStr.
				::DisposePtr((char *) mText);							// toss original text
				mText = (uint8 *) ::NewPtr(finalLen + 1);							// allocate new buffer
				if (mText == nil)
				{
					gLog.Caution("Memory error in text command.");
					return;
				}
				
				strcpy((char *) mText, (char *) finalStr);						// save new string
                return;

            case '^':           //  Hilite char.
                // Now create the list element that tells us what the PREVIOUS
                // style was (now that we know the length)
				CreateStyleEntry(styleStart, styleLen, fHighlite, fUnderline, fBold);
				haveDoneStyle = true;
                ch = 0;             //  Set to 0 so we don't draw it.
                fHighlite = !fHighlite;
                styleStart = finalLen;
                index++;
                
                if (fHighlite)
                {
                	while (mText[index] == ' ')
                		index++;
                }
                styleLen = 0;
                break;

            case '|':		// Underline char - we're switching from _ to | since _ is 
            				// frequently used elsewhere & can't always be escaped easily
                // Now create the list element that tells us what the PREVIOUS
                // style was (now that we know the length)
				CreateStyleEntry(styleStart, styleLen, fHighlite, fUnderline, fBold);
				haveDoneStyle = true;
                ch = 0;
                fUnderline = !fUnderline;
                styleStart = finalLen;
                index++;
                if (fUnderline)
                {
                	while (mText[index] == ' ')
                		index++;
                }
                styleLen = 0;
                break;
                
            case '@':				// Bold char.
            	// Now create the list element that tells us what the PREVIOUS
                // style was (now that we know the length)
				CreateStyleEntry(styleStart, styleLen, fHighlite, fUnderline, fBold);
				haveDoneStyle = true;
                ch = 0;
                fBold = !fBold;
                styleStart = finalLen;
                index++;
                if (fBold)
                {
                	while (mText[index] == ' ')
                		index++;
                }
                styleLen = 0;
                break;

            case '\\':          //  look for newlines and tabs
            	ch = mText[++index];
            	
            	switch (ch)
            	{
            		case 'm':		// mac-only newline
            		case 'n':
						ch = NEWLINE_CHAR;
	            		index++;
						styleLen++;
						break;
						
					case 't':
						// first check that this won't blow out the string
						if ((finalLen + 8) >= kFinalStrLen)
						{
							gLog.Caution("Text too long! More than <%d> characters.", kFinalStrLen);
							::DisposePtr((char *) mText);
							::DisposePtr((char *) finalStr);
							mText = nil;
							return;
						}

						// turn the tab into a run of 8 spaces
						ch = 0;
		            	index++;
						styleLen += 8;		// Increment length of this fragment
						strncpy((char *) &(finalStr[finalLen]), "        ", 8);
						finalLen += 8;
						break;
						
					case '&':
						index++;
						styleLen++;
						break;
						
					case ' ':				// cbo_fix - trim runs of spaces down to a max of 4
						isSpace = true;
						spaceCount++;

						if (spaceCount > 4)
							ch = 0;			// so we will skip them
						else
							styleLen++;		// otherwise, add it to this style
						
						index++;			// look at the next char
					
						break;
						
					case 'w':				// windows-only newline, ignore
						ch = 0;
						index++;
						break;
						
					default:
						index++;
						styleLen++;
						break;
				}
				break;

#ifdef TRY_NOT_DOING_THIS
            case 0xCE:			// don't know why this comes through as a single open smart quote
            case 0xE1:			// single open smart quote
            case 145:			// 0x91
            	ch = 0xD4;
            	index++;
            	styleLen++;
            	break;     
            case 0xE2:			// single close smart quote
            case 146:			// 0x92
              	ch = 0xD5;
            	index++;
            	styleLen++;
            	break;
            case 0xE3:			// double open smart quote
            case 147:			// 0x93
            	ch = 0xD2;
            	index++;
            	styleLen++;
            	break;
            case 0xE4:			// double close smart quote
            case 148:			// 0x94
            	ch = 0xD3;
            	index++;
            	styleLen++;
            	break;
            
            case 0xF7:			// ??	
            case 151:			// em-dash
            	ch = 0xD1;
            	index++;
            	styleLen++;
            	break;
            	
            case 181:			// micro
            	ch = 0xB5;
            	index++;
            	styleLen++;
            	break;	
            	
            case 133:			// elipsis
            	ch = 0xC9;
            	index++;
            	styleLen++;
            	break;
#endif
            	
  			default:
        		index++;
				styleLen++;		// Increment length of this fragment
                break;
        }	// end of 'switch'
        
		if (not isSpace)
			spaceCount = 0;		// reset our space counter
			
		if (ch != 0)					// If it's a printable char...
		{
			if (finalLen >= kFinalStrLen)
			{
				gLog.Caution("Text too long! More than <%d> characters.", kFinalStrLen);
				::DisposePtr((char *) mText);
				::DisposePtr((char *) finalStr);
				mText = nil;
				return;
			}
			
			finalStr[finalLen++] = ch;		// add char to the final string
		}
			        
	}		// end of 'while'

    // Now create the list element that tells us what the PREVIOUS
    // style was (now that we know the length). This creates the final
    // style for the string.
	CreateStyleEntry(styleStart, styleLen, fHighlite, fUnderline, fBold);

	finalStr[finalLen] = '\0';								// terminate finalStr.
	::DisposePtr((char *) mText);							// toss original text
	mText = (uint8 *) ::NewPtr(finalLen + 1);							// allocate new buffer
	if (mText == nil)
	{
		gLog.Caution("Memory error in text command.");
		return;
	}
	
	strcpy((char *) mText, (char *) finalStr);						// save new string
}

/* ---------------------------------------------------------------------------
		¥ DrawText
   ---------------------------------------------------------------------------

	Top-level 'wrapper' for drawing the textbox. First we check to see if there
	is a shadow specified. If so, offset the drawing rect & call the style drawing
	routine. Then reset the rect back to the original coords & call the same
	drawing routine.
	
*/
	
void
CText::DrawText()
{
	if (mText == nil)
		return;
		
	if (mShadowPix != 0)
	{
		::OffsetRect(&mDrawRect, mShadowPix, mShadowPix);
		DrawStyleText(true);
		::OffsetRect(&mDrawRect, -mShadowPix, -mShadowPix);
	}
	
	DrawStyleText(false);
}

/* ---------------------------------------------------------------------------
		¥ DrawStyleText
   ---------------------------------------------------------------------------
	Draw justified text within a rectangle, using the style information stored
	in the style list. This is lifted directly (and then modified) from PowerPlants 
	"UDrawingUtils::DrawWithJustification()" routine. Pass 'TRUE' to draw the
	drop shadow (draws all text in the shadow color).

	During this routine we maintain a pointer to the remainder of the text to be drawn. This
	pointer is incremented when we finish a line, or finish the current style we're drawing.
	We then use this pointer to figure out the next line break.
	
	Sets up text in beginning. then calls LoopThroughStyles to actually do it - EH
*/

void
CText::DrawStyleText(Boolean shadow)
{	
	FontInfo	fontInfo;
	
	if (mText == nil)
		return;
	
	PP::StColorPenState savePenState;
	PP::StColorPenState::Normalize();
	
	// Set font, size, drawing mode (srcOr so don't blast the background pict)
	::TextMode(srcOr);
	::TextFont(mFont);
	::TextSize(mSize);
	
	::TextFace(0);	// reset text
	
	// Get font-related info.
	::GetFontInfo(&fontInfo);
	
	// Figure out how high each line of text is, and also the baseline for the 
	// first line. The baseline is incremented each time we jump to the next line.
	SInt16 lineHeight = fontInfo.ascent + fontInfo.descent + fontInfo.leading;
	SInt16 lineBase = mDrawRect.top + fontInfo.ascent + fontInfo.leading;
	
	// cbo_hack- 
	if ((mFont == kFontIDHelvetica) and (mSize == 18))
		lineBase += 2;
	
	// Set the justification
	SInt16	justification = mJust;
	
	// If we spec default justification, get the default justification from WorldScript
	if (justification == teFlushDefault)
		justification = ::GetSysDirection();

	// Time to run through loop to display text
	LoopThroughStyles(lineHeight, lineBase, justification, shadow);

}
/* ---------------------------------------------------------------------------
		¥ SetupStyle
   ---------------------------------------------------------------------------
	Set style for given section. Called from DrawStyleText and Loop through Styles
*/
void
CText::SetupStyle(SInt32 sIdx, Boolean shadow) 
{
	sTextStyle theStyle;
	if (mStyleOffsets->FetchItemAt(sIdx, &theStyle)) // if no style at this point, do nothing
	{
		if (theStyle.mLen != 0)					// if style has length 0, do nothing 
		{
			// If we're drawing the shadow, set the color to the shadow color.
			RGBColor color;
			if (shadow)
				color = gPaletteManager.GetColor(mShadColor);
			else
				color = gPaletteManager.GetColor(theStyle.mColor);
			::RGBForeColor(&color);	
			SInt16 textStyle = 0;				
	
			if (theStyle.mUnderline)			
				textStyle |= underline;
			if ((mBold) or (theStyle.mBold))	// have two ways of making bold
				textStyle |= bold;
			
			::TextFace(textStyle);				
		}
	}
}



/* ---------------------------------------------------------------------------
		¥ LoopThroughStyles
   ---------------------------------------------------------------------------
	Goes through each style - checks when to line break and draws text for style
	Outer loop is for each line. Inner loop is for each style within line. EHamon
*/
void 
CText::LoopThroughStyles(SInt16	lineHeight, SInt16 lineBase, SInt16 justification, Boolean shadow) 
{	
	Ptr			inText 		= (char *) mText;				// Pointer to text
	SInt32		textLeft	= strlen(inText);				// Number of chars left to display total
	SInt32		textForLine;								// how many bytes total to draw on line
	SInt32		bytesSoFar;									// Number of bytes drawn so far on line
	
	SInt32 		styleIdx 	= 1;							// style we're currently on
	sTextStyle	theStyle;									// stores parameters of current style
	SInt32		styleBytes 	= 0;							// Number of chars drawn so far in current style

	SInt16		incr_y 		= mDrawRect.top + lineHeight;	// starts out at bottom of current line
	SInt16		incr_x		= mDrawRect.left;				// starts out at left of current line
	SInt16		textWidth 	= incr_x;						// pixel line width
	SInt16		textWidthStart = textWidth;
	
	SInt16		loopCount 	= 0;							// catches infinite loops on outer part - determines when to update

	while ((textLeft > 0) and (++loopCount < 256))
	{		
		textWidth = textWidthStart;
		// SetLineBreak returns how much text to print on this line	
		// We set the line break and then do the corresponding justification		
		textForLine = SetLineBreak(inText, styleIdx, styleBytes, textLeft, shadow);
		DoJustification(justification, inText, ::VisibleLength(inText, textForLine), lineBase);	
		
		bytesSoFar = 0;
		while (bytesSoFar < textForLine) 
		{
			// get the new style
			mStyleOffsets->FetchItemAt(styleIdx, &theStyle);
			
			if (theStyle.mLen != 0)	
			{		
				SInt32 drawBytes;							// how many bytes to draw for given style&line
				
				// Set the style for the section
				SetupStyle(styleIdx, shadow);
				
				if ((theStyle.mLen - styleBytes) > (textForLine - bytesSoFar))		// can't draw whole style on line
				{	
					drawBytes = textForLine - bytesSoFar;			 				// so we draw as many as are left
					styleBytes += drawBytes;						 				// we record this so we know for next time
				}
				else
				{					
					drawBytes = theStyle.mLen - styleBytes;					// undrawn part of style fits on line, so we draw it
					styleIdx++;												// now get ready for next style
					styleBytes = 0;	
				}
								
				::DrawText(inText, bytesSoFar, drawBytes);
 				textWidth += ::TextWidth(inText, bytesSoFar, drawBytes);	// increment the right edge of the text
				bytesSoFar += drawBytes;									// increase bytesSoFar;
			}
			else 	// I believe it only gets here when two styles start next to each other - E Hamon
			{
				styleIdx ++;
				gDebugLog.Log("Two styles next to each other? Text address:<%d> Bytes into it: <%d> ", inText, bytesSoFar);
			}
		}
		
		// update parameters since we've finished a line
		inText   += textForLine;					// Move pointer to next line
		textLeft -= textForLine;					// update amount of text left to print
		lineBase += lineHeight;						// Bump the baseline
		
		if (textLeft > 0)							// bump _INCR_Y, if necessary (more lines to go = necessary)
			incr_y += lineHeight;					
		if (textWidth > incr_x)
			incr_x = textWidth;						// bump _INCR_X, if necessary
	}
	if (loopCount >= 256)
		gLog.Error("Too many loops in DrawText");

	gVariableManager.SetLong("_incr_y", (int32) incr_y);
	gVariableManager.SetLong("_incr_x", (int32) incr_x);
	
	if (!shadow) //Douglas only wants it logged once 
		gDebugLog.Log("text rect: L <%d>, T <%d>, R <%d>, B <%d>", mDrawRect.left, mDrawRect.top, incr_x, incr_y);
}

/* ---------------------------------------------------------------------------
		¥ SetLineBreak
   ---------------------------------------------------------------------------
	Set linebreak  for line. Uses mac call StyledLineBreak.
	Called from LoopThroughStyles. Written by E Hamon.
	***NOTE: A \n at the end of the line will not yield a blank line after it becuz of the way StyledLineBreak works.
	Those using the 5L engine should do this manually by increasing incr_y.***
 */
SInt32
CText::SetLineBreak(Ptr inText, SInt32 styleIdx, SInt32 styleBytes, SInt32 textLeft, Boolean shadow)
{	
		SInt32 textStart = 0;				// set values for first run of StyledLineBreak
		SInt32 textOffset = 1;		
		SInt32 textWidth = ::Long2Fix(mDrawRect.right - mDrawRect.left);;
		StyledLineBreakCode	lineBreak;
		sTextStyle	theStyle;

		while (mStyleOffsets->FetchItemAt(styleIdx, &theStyle))
		{	
		 	SInt32 styleBytesLeft = theStyle.mLen - styleBytes;	// bytes left to draw in style
		 	SInt32 textEnd = textStart + styleBytesLeft;
			
			SetupStyle(styleIdx, shadow);
			
			//  textLeft is length of script run (still to be printed - i.e. not include past lines).
			//	inText - ptr to beginning of line.	
			//	bytesSoFar is start of style - offset from beginning of line by bytes printed so far. 
			//	textStart is beginning of style - offset from beginning of line.
			//	textEnd is end of style. (theStyle.mLen - styleBytes) is amount of style still left to be printed
			//	textOffset needs to be set to 0 after first run within loop. 
			//	textOffset is recalculated until all styles on line are examined. 
			//	During last run (when it returns) it returns # of bytes that fit on line
			//	
			//	FOR MORE INFO SEE: http://developer.apple.com/techpubs/mac/Text/Text-332.html for definition
			//	and http://developer.apple.com/techpubs/mac/Text/Text-289.html for sample code 
			lineBreak = StyledLineBreak(inText, textLeft, textStart, textEnd, 0, &textWidth, &textOffset);
		
			if (lineBreak != smBreakOverflow)  	// we have more text than fits on line
				return textOffset; 								
			
			else if (textOffset == textLeft)	// all the remaining text will fit on line
				return textOffset;	
			
			else if (textOffset < textLeft)		// we can draw whole style and still have more text to go and to fit on line
			{									// so we go to next style
				textOffset = 0;
				styleIdx ++;
				styleBytes = 0;
				textStart += styleBytesLeft;
			}
			else
			{
#ifdef DEBUG
				gDebugLog.Error("ERROR! ERROR! StyledLineBreak in SetLineBreak returned incorrectly");	
#endif
				return 0;
			}
		}
		gDebugLog.Log("ERROR! ERROR! StyledLineBreak in SetLineBreak returned incorrectly");
		return 0;
}
		

/* ---------------------------------------------------------------------------
		¥ SetupJustification
   ---------------------------------------------------------------------------
	Set justification for given section. Called from DrawStyleText - originally in it. EHamon
*/
void 
CText::DoJustification(SInt16 justification, Ptr inText, SInt32 blackSpace, SInt16 lineBase) 
{
	switch (justification)
	{
		case AlignLeft:
			::MoveTo(mDrawRect.left, lineBase);	
			break;
			
		case AlignRight:		
			::MoveTo(mDrawRect.right - ::TextWidth(inText, 0, blackSpace), lineBase);
			break;

		case AlignCenter:
			::MoveTo(mDrawRect.left + ((mDrawRect.right - mDrawRect.left) -
					 ::TextWidth(inText, 0, blackSpace)) / 2, lineBase);
			break;
	}
}

/* ---------------------------------------------------------------------------
		¥ DrawSimpleText
   ---------------------------------------------------------------------------
	Draw justified text within a rectangle, using the base or highlight color
	as indicated by the parameter. Performs no styling. Usually called from
	within a button drawing routine.
*/

void
CText::DrawSimpleText(Boolean highlight)
{
	if (mText == nil)
		return;
		
	PP::StColorPenState savePenState;
	PP::StColorPenState::Normalize();
	
	// Set font, size, drawing mode (srcOr so don't blast the background pict)
	::TextMode(srcOr);
	::TextFont(mFont);
	::TextSize(mSize);
	
	if (mBold)
		::TextFace(bold);
	else
		::TextFace(0);
	
	RGBColor color;
	if (highlight)
		color = gPaletteManager.GetColor(mHiColor);
	else
		color = gPaletteManager.GetColor(mBaseColor);
	::RGBForeColor(&color);	
				
	PP::UTextDrawing::DrawWithJustification((char *) mText, strlen((char *) mText), mDrawRect, mJust);
}

/* ---------------------------------------------------------------------------
		¥ CreateStyleEntry
   ---------------------------------------------------------------------------

	Add a style offset entry to the list of offsets.
*/

void
CText::CreateStyleEntry(const int16 inStyleStart,	// Starting offset into mText for this style
						const int16 inStyleLen, 	// Number of chars to display in this style
						const int16 inHighlite,		// TRUE if in highlight color
						const Boolean inUnderline,	// TRUE if text is underlined
						const Boolean inBold)		// TRUE if text is bold
{
	sTextStyle	theStyle = {0, 0, mBaseColor, false, false};
			
	theStyle.mStartPos 	= inStyleStart;			
	theStyle.mLen 		= inStyleLen;			
	
	if (inHighlite)
		theStyle.mColor = mHiColor;		// Color of style
	else
		theStyle.mColor = mBaseColor;	// Color of style

	theStyle.mUnderline = inUnderline;	// TRUE if underline
	theStyle.mBold = inBold;			// TRUE if bold
	
	// Insert at the end of the list.
	mStyleOffsets->InsertItemsAt(1, PP::LArray::index_Last, &theStyle);
}

SInt32
CText::GetLineHeight(void)
{
	FontInfo	fontInfo;
	PP::StTextState	textState;
	
	if (mText == nil)
		return (0);
		
	// Set the font info before we get it. Duh...
	textState.Normalize();
	::TextFont(mFont);
	::TextSize(mSize);
	
	// Get font-related info.
	::GetFontInfo(&fontInfo);
	
	return(fontInfo.ascent + fontInfo.descent + fontInfo.leading);
}