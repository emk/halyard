// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TCommon.h"
#include "TStyleSheet.h"
#include "TVariable.h"
#include "TLogger.h"
#include "TEncoding.h"
#include "TUtilities.h"
#include "TTemplateUtils.h"
#include "TCommonPrimitives.h"

#include <string>
#include <memory>

USING_NAMESPACE_FIVEL

using GraphicsTools::Color;
using Typography::StyledText;
using Typography::TextRenderingEngine;

TStyleSheetManager FIVEL_NS gStyleSheetManager;


//=========================================================================
//  TStyleSheet Methods
//=========================================================================

TStyleSheet::TStyleSheet(TArgumentList &inArgs)
{
    // (defstyle STYLENAME FONTNAME SIZE FLAGS JUSTIFICATION COLOR HIGHCOLOR...
    std::string flags, justification;
    uint32 size;
    inArgs >> SymbolName(mStyleName) >> mFontName >> size >> SymbolName(flags)
		   >> SymbolName(justification) >> mColor >> mHighlightColor;
    mSize = size;
	mStyleName = MakeStringLowercase(mStyleName);

	// Parse our flags value.
	mFaceStyle = Typography::kRegularFaceStyle;
	if (flags == "r")
		mFaceStyle = Typography::kRegularFaceStyle;
	else if (flags == "b")
		mFaceStyle = Typography::kBoldFaceStyle;
	else if (flags == "i")
		mFaceStyle = Typography::kItalicFaceStyle;
	else if (flags == "bi")
		mFaceStyle = Typography::kBoldItalicFaceStyle;
	else
		gLog.Error("Invalid face style '%s'", flags.c_str());

    // Parse our justification value.
	justification = MakeStringLowercase(justification);
    if (justification == "center")
		mJustification = Typography::kCenterJustification;
    else if (justification == "right")
		mJustification = Typography::kRightJustification;
    else
		mJustification = Typography::kLeftJustification;
	
    // ...LEADING...
    if (inArgs.HasMoreArguments())
		inArgs >> ValueOrPercent(mSize, &mLeading);
	else
		mLeading = 0;
	
    // ...SHADOWOFFSET...
    if (inArgs.HasMoreArguments())
		inArgs >> mShadowOffset;
	else
		mShadowOffset = 0;
	
    // ...SHADOWCOLOR...
    if (inArgs.HasMoreArguments())
		inArgs >> mShadowColor;
	else
		mShadowColor = Color(0, 0, 0);

    // ...SHADOWHIGHCOLOR...
    if (inArgs.HasMoreArguments())
		inArgs >> mHighlightShadowColor;
	else
		mHighlightShadowColor = mShadowColor;
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
	base_style.SetFaceStyle(mFaceStyle);
    base_style.SetColor(mColor);
    base_style.SetShadowColor(mShadowColor);
    base_style.SetLeading(mLeading);
    if (mShadowOffset != 0)
    {
		base_style.ToggleFaceStyle(Typography::kShadowFaceStyle);
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
	UpdateSpecialVariablesForText(TPoint(engine.GetRightBound(),
										 engine.GetBottomBound()));

	// TODO - Should these be classified as special variables?
	// They're really just return values for use with MeasureTextAA.
	gVariableManager.SetLong("_text_width", engine.GetTextWidth());
	gVariableManager.SetLong("_text_height", engine.GetTextHeight());
}

int TStyleSheet::GetLineHeight()
{
	// Return the height of the first line.
	return GetBaseStyle().GetLineHeight(true);
}


//=========================================================================
//  TStyleSheetManager Methods
//=========================================================================

TStyleSheet *TStyleSheetManager::Find(const std::string &inName)
{
	std::string name = MakeStringLowercase(inName);
	std::map<std::string,TStyleSheet*>::iterator found =
		mStyleSheetMap.find(name);
	if (found != mStyleSheetMap.end())
		return found->second;
	else
		return NULL;
}

void TStyleSheetManager::AddStyleSheet(TArgumentList &inArgs)
{
	// Create the stylesheet and get the name.
	std::auto_ptr<TStyleSheet> sheet =
		std::auto_ptr<TStyleSheet>(new TStyleSheet(inArgs));
	std::string name = sheet->GetName();

	// Check for an exiting stylesheet with the same name.
	if (Find(name))
	{
		gLog.Error("Can't redefine style sheet <%s>.", name.c_str());
		return;
	}

	// Insert the new stylesheet in our map.
	mStyleSheetMap.insert(std::pair<std::string,TStyleSheet*>(name,
															  sheet.release()));
}

void TStyleSheetManager::RemoveAll()
{
	// Delete the individual stylesheets and empty the map.
	std::map<std::string,TStyleSheet*>::iterator iter =
		mStyleSheetMap.begin();
	for (; iter != mStyleSheetMap.end(); ++iter)
		delete iter->second;
	mStyleSheetMap.clear();
}

void TStyleSheetManager::Draw(const std::string &inStyleSheet,
							  const std::string& inText,
							  GraphicsTools::Point inPosition,
							  GraphicsTools::Distance inLineLength,
							  GraphicsTools::Image *inImage)
{
	TStyleSheet *style_sheet = Find(inStyleSheet);
	if (!style_sheet)
	{
		gDebugLog.Caution("Tried to draw text using non-existant style "
						  "sheet <%s>", inStyleSheet.c_str());
		return;
	}
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
	TStyleSheet *style_sheet = Find(inStyleSheet);
	if (!style_sheet)
		gLog.FatalError("Tried to measure height of non-existant style "
						"sheet <%s>", inStyleSheet);
	return style_sheet->GetLineHeight();
}
