// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TStyleSheet_H
#define TStyleSheet_H

#include <string>
#include <map>

#include "TCommon.h"
#include "TPrimitives.h"

#include "GraphicsTools.h"
#include "Typography.h"

// XXX - Huh?  Who included by Typography.h (on Win32) is defining DrawText
// and other crufy Win32 preprocessor stuff?
#undef DrawText
#undef GetObject

BEGIN_NAMESPACE_FIVEL

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
	// Get a Typography::Style object corresponding to unhighlighted
	// text.
	//
	Typography::Style GetBaseStyle();

public:
	//////////
	// Create a new style sheet from an argument list.
	//
	TStyleSheet(TArgumentList &inArgs);
	
	//////////
	// Get the name of this style sheet.
	//
	std::string GetName() const { return mStyleName; }

	//////////
	// Convert a 5L-format string into a StyledText object, using the
	// data stored in this style.  This is a pretty nasty formatting
	// system, and it handles a lot of escapes which should be processed
	// by TStream instead.
	//
	Typography::StyledText MakeStyledText(const std::string& inText);
	
	//////////
	// Draw text onto the specified image.
	//
	// [in] inText - The text to draw, with standard 5L formatting.
	// [in] inPosition - The upper-left corner of the text box.
	// [in] inLineLength - The maximum number of pixels available for
	//                     a line.  This is (I hope) a hard limit,
	//                     and no pixels should ever be drawn beyond it.
	// [in] inImage -      The image into which we should draw.
	//                     This must not be deallocated until the
	//                     TextRendering engine is destroyed.  May be
	//                     NULL if we only want to measure the text.
	//
	void Draw(const std::string& inText,
			  GraphicsTools::Point inPosition,
			  GraphicsTools::Distance inLineLength,
			  GraphicsTools::Image *inImage);

	//////////
	// Get the height of a single line of text drawn in this style.
	//
	int GetLineHeight();
};

class TStyleSheetManager
{
	std::map<std::string,TStyleSheet*> mStyleSheetMap;

public:
	virtual ~TStyleSheetManager() { RemoveAll(); }

	//////////
	// Return the specified stylesheet, or NULL.
	//
	TStyleSheet *Find(const std::string &inName);

	//////////
	// Create a new style sheet using the supplied parameters.
	//
	void AddStyleSheet(TArgumentList &inArgs);

	//////////
	// Remove all the stylesheets from this object.
	//
	void RemoveAll();

	//////////
	// Draw text onto the specified image, using the specified
	// style sheet.
	//
	// [in] inStyleSheet - The name of the style sheet to use.
	// [in] inText - The text to draw, with standard 5L formatting.
	// [in] inPosition - The upper-left corner of the text box.
	// [in] inLineLength - The maximum number of pixels available for
	//                     a line.  This is (I hope) a hard limit,
	//                     and no pixels should ever be drawn beyond it.
	// [in] inImage -      The image into which we should draw.
	//                     This must not be deallocated until the
	//                     TextRendering engine is destroyed.  May be
	//                     NULL if we only want to measure the text.
	//
	void Draw(const std::string &inStyleSheet,
			  const std::string &inText,
			  GraphicsTools::Point inPosition,
			  GraphicsTools::Distance inLineLength,
			  GraphicsTools::Image *inImage);

	//////////
	// Compatibility function.  This call works like the above,
	// but takes arguments in the same order as the old Win32
	// Header class.  It makes the code prettier.
	//
	void DoText(const char *inStyleSheet, TRect inRect,
				const char *inText, GraphicsTools::Image *inImage);

	//////////
	// Compatibility function.  Get the height of the first line of
	// the text.
	//
	int GetLineHeight(const char *inStyleSheet);
};

extern TStyleSheetManager gStyleSheetManager;

END_NAMESPACE_FIVEL

#endif // TStyleSheet_H
