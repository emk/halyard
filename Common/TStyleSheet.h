// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

#ifndef TStyleSheet_H
#define TStyleSheet_H

#include "TPrimitives.h"
#include "Typography.h"


// XXX - Huh?  Who included by Typography.h (on Win32) is defining DrawText
// and other crufy Win32 preprocessor stuff?
#undef DrawText
#undef GetObject

BEGIN_NAMESPACE_FIVEL

class xml_node;

/// A stylesheet for text displayed by a script.
class TStyleSheet {
	// (defstyle STYLENAME FONTNAME SIZE FLAGS JUSTIFICATION COLOR HIGHCOLOR
	//           [LEADING [SHADOWOFFSET SHADOWCOLOR [SHADOWHIGHCOLOR]]])
	std::string          mStyleName;
	std::string          mFontName;
	int                  mSize;
	Typography::FaceStyle mFaceStyle;
	Typography::Justification mJustification;
	GraphicsTools::Color mColor;
	GraphicsTools::Color mHighlightColor;
	Typography::Distance mLeading;
	Typography::Distance mShadowOffset;
	GraphicsTools::Color mShadowColor;
	GraphicsTools::Color mHighlightShadowColor;

	//////////
	/// Get a Typography::Style object corresponding to unhighlighted
	/// text.
	///
	Typography::Style GetBaseStyle();

    //////////
    /// Convert all the children of an XML node to styled text.
    ///
    void ProcessNodeChildren(xml_node &inNode,
                             std::vector<Typography::Style> &ioStyleStack,
							 Typography::StyledText &outText);

    //////////
    /// Convert an XML node to styled text.
    ///
    void ProcessNode(xml_node &inNode,
                     std::vector<Typography::Style> &ioStyleStack,
                     Typography::StyledText &outText);

public:
	//////////
	/// Create a new style sheet from an argument list.
	///
	TStyleSheet(TArgumentList &inArgs);
	
	//////////
	/// Get the name of this style sheet.
	///
	std::string GetName() const { return mStyleName; }

	//////////
	/// Convert a 5L-format string into a StyledText object, using the
	/// data stored in this style.  This is a pretty nasty formatting
	/// system, and it handles a lot of escapes which should be processed
	/// by TStream instead.
	///
    /// \deprecated Replace with an XML-based styled text system.
    ///
	Typography::StyledText MakeStyledText(const std::string& inText);
	
	//////////
	/// Draw text onto the specified image.
	///
	/// \param inText  The text to draw, with standard 5L formatting.
	/// \param inPosition  The upper-left corner of the text box.
	/// \param inLineLength  The maximum number of pixels available for
	///                     a line.  This is (I hope) a hard limit,
	///                     and no pixels should ever be drawn beyond it.
	/// \param inImage  The image into which we should draw.
	///                     This must not be deallocated until the
	///                     TextRendering engine is destroyed.  May be
	///                     NULL if we only want to measure the text.
	///
	TRect Draw(const std::string& inText,
               GraphicsTools::Point inPosition,
               GraphicsTools::Distance inLineLength,
               GraphicsTools::Image *inImage);

	//////////
	/// Get the height of a single line of text drawn in this style.
	///
	int GetLineHeight();
};

/// Centralized manager class for TStyleSheet objects.
class TStyleSheetManager
{
	std::map<std::string,TStyleSheet*> mStyleSheetMap;

public:
	virtual ~TStyleSheetManager() { RemoveAll(); }

	//////////
	/// Return the specified stylesheet, or NULL.
	///
	TStyleSheet *Find(const std::string &inName);

	//////////
	/// Create a new style sheet using the supplied parameters.
	///
	void AddStyleSheet(TArgumentList &inArgs);

	//////////
	/// Remove all the stylesheets from this object.
	///
	void RemoveAll();

	//////////
	/// Draw text onto the specified image, using the specified
	/// style sheet.
	///
	/// \param inStyleSheet  The name of the style sheet to use.
	/// \param inText  The text to draw, with standard 5L formatting.
	/// \param inPosition  The upper-left corner of the text box.
	/// \param inLineLength  The maximum number of pixels available for
	///                     a line.  This is (I hope) a hard limit,
	///                     and no pixels should ever be drawn beyond it.
	/// \param inImage  The image into which we should draw.
	///                     This must not be deallocated until the
	///                     TextRendering engine is destroyed.  May be
	///                     NULL if we only want to measure the text.
	///
	TRect Draw(const std::string &inStyleSheet,
               const std::string &inText,
               GraphicsTools::Point inPosition,
               GraphicsTools::Distance inLineLength,
               GraphicsTools::Image *inImage);

	//////////
	/// Compatibility function.  Get the height of the first line of
	/// the text.
	///
	int GetLineHeight(const char *inStyleSheet);
};

extern TStyleSheetManager gStyleSheetManager;

END_NAMESPACE_FIVEL

#endif // TStyleSheet_H
