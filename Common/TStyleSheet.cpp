// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#include "CommonHeaders.h"
#include "TStyleSheet.h"
#include "TTextTransform.h"
#include "TCommonPrimitives.h"
#include "XmlUtils.h"

#include <string>
#include <memory>

#include <libxml/tree.h>
#include <libxml/parser.h>

USING_NAMESPACE_FIVEL

using GraphicsTools::Color;
using Typography::Style;
using Typography::StyledText;
using Typography::TextRenderingEngine;

TStyleSheetManager FIVEL_NS gStyleSheetManager;


//=========================================================================
//  Standard Entities
//=========================================================================
//  These are the named entities which we pass to our XML parser.  There's
//  no particular rhyme or reason to the this list--it includes a few
//  characters which are needed by various internal routines, and a list
//  of common entities which we've found handy.
//
//  Feel free to add more, but we'd like to keep this list to a few
//  hundred, tops.

const char *STANDARD_ENTITIES =
// Standard ASCII entities.  Needed to bypass automatic smart quotes, etc.
"<!ENTITY apos   \"'\">\n"
"<!ENTITY hyphen '-'>\n"
"<!ENTITY period '.'>\n"

// Internally-referenced entities.  Used as output for smart-quotes, etc.
"<!ENTITY mdash  '&#x2014;'>\n"
"<!ENTITY lsquo  '&#x2018;'>\n"
"<!ENTITY rsquo  '&#x2019;'>\n"
"<!ENTITY ldquo  '&#x201C;'>\n"
"<!ENTITY rdquo  '&#x201D;'>\n"
"<!ENTITY hellip '&#x2026;'>\n"

// Other entities.  Here's a nice table of HTML entity names:
//   http://www.cs.tut.fi/~jkorpela/html/guide/entities.html
"<!ENTITY copy   '&#x00A9;'>\n"
"<!ENTITY reg    '&#x00AE;'>\n"
"<!ENTITY trade  '&#x2122;'>\n"
"<!ENTITY bull   '&#x2022;'>\n"
"<!ENTITY sect   '&#x00A7;'>\n"
"<!ENTITY ndash  '&#x2013;'>\n"
"<!ENTITY dagger '&#x2020;'>\n"
"<!ENTITY micro  '&#x00B5;'>\n"
"<!ENTITY para   '&#x00B6;'>\n"
"<!ENTITY eacute '&#x00E9;'>\n"
"<!ENTITY Ccedil '&#x00C7;'>\n"
"<!ENTITY ccedil '&#x00E7;'>\n"
"<!ENTITY egrave '&#x00E8;'>\n" // E with grave accent.
"<!ENTITY nbsp   '&#x00A0;'>\n" // Non-breaking space.
"<!ENTITY shy    '&#x00AD;'>\n" // Soft hyphen.
"<!ENTITY deg    '&#x00B0;'>\n" // Degree sign.
"<!ENTITY radic  '&#x221A;'>\n" // Square root.
"<!ENTITY check  '&#x2713;'>\n" // Check mark (may not have font support).
"<!ENTITY cross  '&#x2717;'>\n" // Ballot X (may not have font support).
"<!ENTITY Delta  '&#x2206;'>\n" // Unicode INCREMENT character.
"<!ENTITY alpha  '&#x03B1;'>\n" // Lowercase greek alpha.
"<!ENTITY beta   '&#x03B2;'>\n" // Lowercase greek beta.
"<!ENTITY gamma  '&#x03B3;'>\n" // Lowercase greek gamma.
"<!ENTITY delta  '&#x03B4;'>\n" // Lowercase greek delta.
"<!ENTITY lambda '&#x03BB;'>\n" // Lowercase greek lambda.
"<!ENTITY pi     '&#x03C0;'>\n" // Lowercase greek pi.
"<!ENTITY infin  '&#x221E;'>\n" // Infinity.
"<!ENTITY ne     '&#x2260;'>\n" // Not equal sign.
"<!ENTITY AElig  '&#x00C6;'>\n" // AE ligature.
"<!ENTITY aelig  '&#x00E6;'>\n" // ae ligature.
"<!ENTITY OElig  '&#x0152;'>\n" // OE ligature.
"<!ENTITY oelig  '&#x0153;'>\n" // oe ligature.
"<!ENTITY sup1   '&#x00B9;'>\n" // Superscript 1.
"<!ENTITY sup2   '&#x00B2;'>\n" // Superscript 2.
"<!ENTITY sup3   '&#x00B3;'>\n" // Superscript 3.
"<!ENTITY frac14 '&#x00BC;'>\n" // 1/4.
"<!ENTITY frac12 '&#x00BD;'>\n" // 1/2.
"<!ENTITY frac34 '&#x00BE;'>\n" // 3/4.
"<!ENTITY plusmn '&#x00B1;'>\n" // +-
"<!ENTITY there4 '&#x2234;'>\n" // "Therefore" (three-dot triangle).
"<!ENTITY times  '&#x00D7;'>\n" // Multiplication.
"<!ENTITY divide '&#x00F7;'>\n" // Division.
"<!ENTITY euro   '&#x8364;'>\n" // Euro (may not have font support).
;


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

Typography::StyledText TStyleSheet::MakeStyledText(const std::string& inText)
{
	// Convert string to Unicode, and handle smart quotes, em-dashes, etc.
	utf8_string expanded = utf8_from_multibyte(inText);
	utf8_string xformed = TTextTransform::TransformString(expanded);

    // Build a plausible-looking XML document.
    utf8_string doc_src =
        "<?xml version='1.0' encoding='utf-8' ?>\n"
		"<!DOCTYPE text [\n" + utf8_string(STANDARD_ENTITIES) + "]>\n"
        "<text>" + xformed + "</text>";

    // Create a styled text object, and initialize our stack of active text
    // styles.  Note that we never pop this first element.
	Typography::Style base_style = GetBaseStyle();
    StyledText text(base_style);
    std::vector<Style> style_stack;
    style_stack.push_back(base_style);

    // Parse our XML document.
    xmlDocPtr doc = xmlParseMemory(&doc_src[0], doc_src.size());
	try {
        CHECK(doc, "Can't parse text: {{" + inText + "}}");

		// Get the root element.
		xmlNodePtr root_node = xmlDocGetRootElement(doc);
		CHECK(root_node, "No document root in XML file");
		xml_node root(root_node);
        ASSERT(root.name() == "text");
        
        // Turn the root's child nodes into a styled text string.
        ProcessNodeChildren(root, style_stack, text);
    } catch (...) {
		if (doc)
			xmlFreeDoc(doc);
		throw;
	}
	xmlFreeDoc(doc);

    // Finish constructing our styled text and return it.
    ASSERT(style_stack.size() == 1);
	text.EndConstruction();
	return text;
}

void TStyleSheet::ProcessNodeChildren(xml_node &inNode,
                                      std::vector<Style> &ioStyleStack,
                                      StyledText &outText)
{
    xml_node::iterator i = inNode.begin_mixed();
	for (; i != inNode.end_mixed(); ++i)
        ProcessNode(*i, ioStyleStack, outText);
}

void TStyleSheet::ProcessNode(xml_node &inNode,
                              std::vector<Style> &ioStyleStack,
                              StyledText &outText)
{
    if (inNode.is_content_node()) {
        outText.AppendText(inNode.content());
    } else if (inNode.is_element_node()) {
        // Update our style based on the information in this element.
        Style style = ioStyleStack.back();
        std::string name(inNode.name());
        if (name == "i" || name == "em" || name == "cite") {
            // TODO - Do we want to use ToggleFaceStyle or SetFaceStyle?
            style.ToggleFaceStyle(Typography::kItalicFaceStyle);
        } else if (name == "b" || name == "strong") {
            style.SetFaceStyle(style.GetFaceStyle() |
                               Typography::kBoldFaceStyle);
        } else if (name == "u") {
            style.SetFaceStyle(style.GetFaceStyle() |
                               Typography::kUnderlineFaceStyle);
        } else if (name == "h") {
            // Our "highlight" tag.  Our content authors like to use a a
            // colored highlight for a number of different purposes, so
            // we're including a non-standard tag which does just that.
            style.SetColor(mHighlightColor);
            style.SetShadowColor(mHighlightShadowColor);
        } else {
            THROW("Unsupported text element: " + name);
        }

        // Install our new style.
        outText.ChangeStyle(style);
        ioStyleStack.push_back(style);

        // Recursively process our child nodes.
        ProcessNodeChildren(inNode, ioStyleStack, outText);

        // Restore our previous text style.
        ioStyleStack.pop_back();
        outText.ChangeStyle(ioStyleStack.back());
    } else {
        THROW("Unexpected XML node type in text");
    }
}

TRect TStyleSheet::Draw(const std::string& inText,
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

    // Return the bounding box of the text we drew.
    GraphicsTools::Distance left, top, right, bottom;
    engine.GetTextBounds(left, top, right, bottom);
    return TRect(left, top, right, bottom);
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

TRect TStyleSheetManager::Draw(const std::string &inStyleSheet,
                               const std::string& inText,
                               GraphicsTools::Point inPosition,
                               GraphicsTools::Distance inLineLength,
                               GraphicsTools::Image *inImage)
{
	TStyleSheet *style_sheet = Find(inStyleSheet);
	if (!style_sheet)
	{
		gLog.Error("Tried to draw text using non-existant style "
				   "sheet <%s>", inStyleSheet.c_str());
		return TRect(0, 0, 0, 0);
	}
	return style_sheet->Draw(inText, inPosition, inLineLength, inImage);
}

int TStyleSheetManager::GetLineHeight(const char *inStyleSheet)
{
	TStyleSheet *style_sheet = Find(inStyleSheet);
	if (!style_sheet)
		gLog.FatalError("Tried to measure height of non-existant style "
						"sheet <%s>", inStyleSheet);
	return style_sheet->GetLineHeight();
}
