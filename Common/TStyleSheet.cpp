// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TCommon.h"
#include "TStyleSheet.h"
#include "TVariable.h"
#include "TLogger.h"
#include "TEncoding.h"
#include "TUtilities.h"
#include "TTemplateUtils.h"

#include <string>

USING_NAMESPACE_FIVEL

using GraphicsTools::Color;
using Typography::StyledText;
using Typography::TextRenderingEngine;

static const char *INCR_Y_NAME = "_incr_y";
static const char *INCR_X_NAME = "_incr_x";

TStyleSheetManager FIVEL_NS gStyleSheetManager;


//=========================================================================
//  TStyleSheet Methods
//=========================================================================

TStyleSheet::TStyleSheet(TIndexFile *inFile, const char *inName,
						 int32 inStart, int32 inEnd)
	: TIndex(inFile, inName, inStart, inEnd)
{
	// Read in our script.
	if (!SetScript())
		gLog.FatalError("I/O error reading script for %s", inName);
	TStream stream(GetScript());

    // (defstyle STYLENAME FONTNAME SIZE JUSTIFICATION COLOR HIGHCOLOR...
    std::string justification;
    uint32 size;
    stream >> open >> discard >> mStyleName >> mFontName >> size
		   >> justification >> mColor >> mHighlightColor;
    mSize = size;

    // Parse our justification value.
	justification = MakeStringLowercase(justification);
    if (justification == "center")
		mJustification = Typography::kCenterJustification;
    else if (justification == "right")
		mJustification = Typography::kRightJustification;
    else
		mJustification = Typography::kLeftJustification;
	
    // ...LEADING...
    if (stream.more())
		stream >> ValueOrPercent(mSize, &mLeading);
	else
		mLeading = 0;
	
    // ...SHADOWOFFSET...
    if (stream.more())
		stream >> mShadowOffset;
	else
		mShadowOffset = 0;
	
    // ...SHADOWCOLOR...
    if (stream.more())
		stream >> mShadowColor;
	else
		mShadowColor = Color(0, 0, 0);

    // ...SHADOWHIGHCOLOR...
    if (stream.more())
		stream >> mHighlightShadowColor;
	else
		mHighlightShadowColor = mShadowColor;

    // ...)
    stream >> close;
    
    // Release our script data.
    FlushScript();
}

Typography::Style TStyleSheet::GetBaseStyle()
{
	// Build a Typography::Style object based on our style sheet.
	// We'll use this as our "base style" when drawing.
    Typography::Style base_style(mFontName, mSize);
	std::list<std::string> backups;
	backups.push_back("Standard Symbols L");
	backups.push_back("Dingbats");
	base_style.SetBackupFamilies(backups);
    base_style.SetColor(mColor);
    base_style.SetShadowColor(mShadowColor);
    base_style.SetLeading(mLeading);
    if (mShadowOffset != 0)
    {
		base_style.SetFaceStyle(Typography::kShadowFaceStyle);
		base_style.SetShadowOffset(mShadowOffset);
    }
	return base_style;
}

static void LogEncodingErrors (const std::wstring &inBadStr, size_t inBadPos,
							   const char *inErrMsg)
{
	std::string bad_string =
		ConstructString<char,std::wstring::const_iterator>(inBadStr.begin(),
														   inBadStr.end());
	gLog.Caution("ENCODING WARNING: %s at position %d in string <<%s>>.",
				 inErrMsg, inBadPos, bad_string.c_str());
}

Typography::StyledText TStyleSheet::MakeStyledText(const std::string& inText)
{
	// Convert 7-bit to 8-bit code.
	// See the notes about TEncoding; it desperately needs refactoring.
	TEncoding<wchar_t> encoding("UTF-16", &LogEncodingErrors);
	std::wstring expanded =
		ConstructString<wchar_t,std::string::const_iterator>(inText.begin(),
															 inText.end());
	std::wstring encoded = encoding.TransformString(expanded);

    // Create a styled text object.
	Typography::Style base_style = GetBaseStyle();
    Typography::Style style = base_style;
    StyledText text(style);
	
    // Process each character.
    bool is_hightlight = false;
    std::wstring::const_iterator cp = encoded.begin();
    for (; cp < encoded.end(); ++cp)
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
			
			
			case '\\': // Escape sequence
				if (++cp == encoded.end())
					throw TException(__FILE__, __LINE__,
									 "Incomplete escape sequence in \"" +
									 inText + "\"");
				switch (*cp)
				{
					case 'n':
						text.AppendText('\n');
						break;
						
					case 't':
						text.AppendText('\t');
						break;
						
					case '^':
					case '|':
					case '_':
					case '@':
					case ' ':
					case '(':
					case ')':
					case '\\':
					case '$':
					case '#':
						text.AppendText(*cp);
						break;
						
					default:
						// XXX - This is really, unbelievably heinous.
						// CStream does not remove its escape sequences
						// from the strings it gives us, so we need to
						// remove those escape sequences ourselves.
						// Unfortunately, to preserve bug-for-bug
						// compatibility, we must do it *after* we
						// encode our entities into 16-bit characters.
						// Net result: "\&amp;" becomes "\&" during
						// entity expansion!  So we can't print the backslash
						// or issue warnings for unknown escape sequences
						// because virtually any character could legitimately
						// appear after '\'.
						
						//text.AppendText('\\');
						text.AppendText(*cp);
						//gDebugLog.Caution("Unrecognized escape \\%c", *cp);
					
				}
				break;
				
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

int TStyleSheet::GetLineHeight()
{
	// Return the height of the first line.
	return GetBaseStyle().GetLineHeight(true);
}


//=========================================================================
//  TStyleSheetManager Methods
//=========================================================================

void TStyleSheetManager::MakeNewIndex(TIndexFile *inFile, const char *inName,
							  		  int32 inStart, int32 inEnd)
{
	Add(new TStyleSheet(inFile, inName, inStart, inEnd));
}

void TStyleSheetManager::Draw(const std::string &inStyleSheet,
							  const std::string& inText,
							  GraphicsTools::Point inPosition,
							  GraphicsTools::Distance inLineLength,
							  GraphicsTools::Image *inImage)
{
	TBNode *node = Find(inStyleSheet.c_str());
	if (!node)
	{
		gDebugLog.Caution("Tried to draw text using non-existant style "
						  "sheet <%s>", inStyleSheet.c_str());
		return;
	}
	TStyleSheet *style_sheet = dynamic_cast<TStyleSheet*>(node);
	style_sheet->Draw(inText, inPosition, inLineLength, inImage);
}

void TStyleSheetManager::DoText(const char *inStyleSheet, TRect inRect,
								const char *inText,
								GraphicsTools::Image *inImage)
{
	Draw(inStyleSheet, inText, GraphicsTools::Point(inRect.Left(),
													inRect.Top()),
		 inRect.Right() - inRect.Left(), inImage);
}

int TStyleSheetManager::GetLineHeight(const char *inStyleSheet)
{
	TBNode *node = Find(inStyleSheet);
	if (!node)
		gLog.FatalError("Tried to measure height of non-existant style "
						"sheet <%s>", inStyleSheet);
	TStyleSheet *style_sheet = dynamic_cast<TStyleSheet*>(node);
	return style_sheet->GetLineHeight();
}
