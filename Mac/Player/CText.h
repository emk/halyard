/* =================================================================================
	CText.h	
	
	Draw a box in the player view. Can define bounds, fill (frames if no fill),
	line width & color. Base class: LPane. 
	
	This is the header for a custom Player Pane class. To define a new class:
	- declare the base class in the class header
	- define the class ID
	- define the approriate constructors. The 'SPaneInfo' constructor will need
	  to have the same parameters as the base classes SPaneInfo constructor.
	- Define destructor
	- Define any data members
	- Override at least DrawSelf().
   ================================================================================= */

#pragma once
#include <TextEdit.h>
#include <LArray.h>

class CText 
{
	public:
		enum { class_ID = 'Text' };	// Class ID - needs to be unique & not all lower-case

		// Standard constructors.
		CText(void);
		CText(	const char	*inHeader,	// Name of header format to use.
				const KRect	&inBounds,	// Bouding rect (relative to PlayerView's rect)
				const char 	*inText);	// The 'raw' string (including format chars)
					
		// Destructor
		virtual 			~CText();
		void				DrawSimpleText(Boolean highlight);
		SInt32				GetLineHeight(void);	// Returns lineheight of text object

	protected:
		LArray				*mStyleOffsets;	// List of styles & offsets into mText for display

		uint8				*mText;		// Pointer to the text to display
		int16				mBaseColor, mHiColor, mShadColor;	// Base, highlight & shadow colors
		int16				mFont;		// Font index
		int16				mSize;		// Point size
		int16				mJust;		// Alignment
		Rect				mDrawRect;	// Text drawing rectangle
		int8				mShadowPix;	// # of pixels to offset drop shadow
		Boolean				mBold;
			
		void 				SetupText();
		void 				CreateStyleEntry(const int16 inStyleStart,	// Starting offset into mText for this style
											 const int16 inStyleLen, 	// Number of chars to display in this style
											 const int16 inHighlite,	// TRUE if in highlight color
											 const Boolean inUnderline,	// TRUE if text is underlined
											 const Boolean inBold);		// TRUE if text is bold
		void		DrawText();	
		void		DrawStyleText(Boolean underline);	
};

/*	sTextStyle
		
	This is a type used to define a list of offsets into CText.mText. Each offset 
	contains a series of attributes which defines the style in which the text (as
	defined by 'mStartPos' & 'mLen') is to be drawn. */
	
struct sTextStyle {
	int16	mStartPos;	// Starting offset into mText for this style
	int16	mLen;		// Number of chars to display in this style
	int16	mColor;		// Color of style
	Boolean	mUnderline;	// TRUE if underline
	Boolean	mBold;		// TRUE if bold
};