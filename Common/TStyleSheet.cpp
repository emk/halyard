// -*- Mode: C++; tab-width: 4; -*-

#include "TStyleSheet.h"
#include "TVariable.h"

USING_NAMESPACE_FIVEL

using GraphicsTools::Color;
using Typography::Style;
using Typography::StyledText;
using Typography::TextRenderingEngine;

static const char *INCR_Y_NAME = "_incr_y";
static const char *INCR_X_NAME = "_incr_x";


//=========================================================================
//  TStyleSheet Methods
//=========================================================================

TStyleSheet::TStyleSheet(TStream &inStream)
{
    // (defstyle STYLENAME FONTNAME SIZE JUSTIFICATION COLOR HIGHCOLOR...
    std::string justification;
    uint32 size;
    inStream >> open >> discard >> mStyleName >> mFontName >> size
			 >> justification >> mColor >> mHighlightColor;
    mSize = size;

    // Parse our justification value.
    std::transform(justification.begin(), justification.end(),
				   justification.begin(), tolower);
    if (justification == "center")
		mJustification = Typography::kCenterJustification;
    else if (justification == "right")
		mJustification = Typography::kRightJustification;
    else
		mJustification = Typography::kLeftJustification;
	
    // Set sensible defaults for our optional arguments.
    mLeading = 0;
    mShadowOffset = 0;
    mShadowColor = Color(0, 0, 0, 0);
    mHighlightShadowColor = mHighlightColor;
	
    // ...LEADING... [ []]])
    if (inStream.more())
		inStream >> mLeading;
	
    // ...SHADOWOFFSET SHADOWCOLOR...
    if (inStream.more())
		inStream >> mShadowOffset >> mShadowColor;
	
    // ...SHADOWHIGHCOLOR...
    if (inStream.more())
		inStream >> mHighlightShadowColor;

    // ...)
    inStream >> close;
}

Typography::StyledText TStyleSheet::MakeStyledText(const std::string& inText)
{
    // Create our base style for non-highlighted text.
    Style base_style(mFontName, mSize);
    base_style.SetColor(mColor);
    base_style.SetShadowColor(mShadowColor);
    base_style.SetLeading(mLeading);
    if (mShadowOffset != 0)
    {
		base_style.SetFaceStyle(Typography::kShadowFaceStyle);
		base_style.SetShadowOffset(mShadowOffset);
    }
	
    // Create a styled text object.
    Style style(base_style);
    StyledText text(style);
	
    // XXX - Skip leading whitespace (to preserve backwards compatibility).
	// This should probably go away, or happen in a much higher layer.
    std::string::const_iterator cp = inText.begin();
    while (cp < inText.end() && *cp == ' ')
		++cp;
	
    // Process each character.
    bool is_hightlight = false;
    for (; cp < inText.end(); ++cp)
    {
		switch (*cp)
		{
			case '^': // Highlight
				if (is_hightlight)
				{
					style.SetColor(mColor);
					style.SetShadowColor(mShadowColor);
				}
				else
				{
					style.SetColor(mHighlightColor);
					style.SetShadowColor(mHighlightShadowColor);
				}
				is_hightlight = !is_hightlight;
				text.ChangeStyle(style);
				break;
				
			case '|': // Underline
				// TODO - Change from italic to underline once
				// underline works.
				style.ToggleFaceStyle(Typography::kItalicFaceStyle);
				text.ChangeStyle(style);
				break;
				
			case '@': // Bold
				style.ToggleFaceStyle(Typography::kBoldFaceStyle);
				text.ChangeStyle(style);
				break;
				
            // TODO - We're still missing a lot of features from Header.cpp.
			default: // Regular character.
				text.AppendText(*cp);
		}
    }
	
	text.EndConstruction();
	return text;
}

void TStyleSheet::Draw(const std::string& inText,
					   GraphicsTools::Point inPosition,
					   GraphicsTools::Distance inLineLength,
					   GraphicsTools::Image *inImage)
{
	StyledText text = MakeStyledText(inText);
	TextRenderingEngine engine(text, inPosition, inLineLength,
							   mJustification, inImage);
	engine.RenderText();
    gVariableManager.SetLong(INCR_Y_NAME, engine.GetBottomBound()); 
	gVariableManager.SetLong(INCR_X_NAME, engine.GetRightBound());
}
