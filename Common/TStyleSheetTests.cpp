// -*- Mode: C++; tab-width: 4; -*-

#include <iostream>
#include <strstream>
#include <string>

#include "ImlUnit.h"
#include "TCommon.h"
#include "TStyleSheet.h"
#include "TPrimitives.h"

USING_NAMESPACE_FIVEL

using namespace Typography;

extern void test_TStyleSheet(void);

class TDummyArgumentList : public TArgumentList
{
	int mCount;
	std::istrstream mData;

public:
	TDummyArgumentList()
		: mCount(11), // Number of tokens in mData
		  mData("S1 Times 12 r right 0xFFFFFF00 0x00FF0000 -1 2 "
				"0x00000080 0x00FF0080") {}

public:
	virtual bool HasMoreArguments() { return mCount > 0; }

protected:
	// This is a macro, not a template, because MSVC 6 sucks.
#define DECLARE_GET_ARG(NAME, TYPE) \
	TYPE NAME() { \
		ASSERT(mCount > 0); \
		TYPE result; \
		mData >> result; \
		mCount--; \
		return result; \
	}

	DECLARE_GET_ARG(GetStringArg, std::string)
	DECLARE_GET_ARG(GetSymbolArg, std::string)
	DECLARE_GET_ARG(GetInt32Arg, int32)
	DECLARE_GET_ARG(GetUInt32Arg, uint32)
	virtual bool GetBoolArg() { return GetInt32Arg() ? true : false; }
	DECLARE_GET_ARG(GetDoubleArg, double);
	virtual TPoint GetPointArg() { ASSERT(false); return TPoint(0, 0); }
	virtual TRect GetRectArg() { ASSERT(false); return TRect(0, 0, 0, 0); }
	virtual GraphicsTools::Color GetColorArg()
	{
		mData >> std::hex;
		uint32 color = GetUInt32Arg();
		mData >> std::dec;
		return GraphicsTools::Color((color & 0xFF000000) >> 24,
									(color & 0x00FF0000) >> 16,
									(color & 0x0000FF00) >> 8,
									(color & 0x000000FF));
	}
	virtual void GetValueOrPercentArg(bool &outIsPercent, int32 &outValue)
	{
		outIsPercent = false;
		outValue = GetInt32Arg();
	}
	virtual TCallback *GetCallbackArg() { ASSERT(false); return NULL; }
	virtual TArgumentList *GetListArg() { ASSERT(false); return NULL; }
};

static void test_style(const StyledText &inText, int inBegin, int inEnd,
					   const Typography::Style &inDesiredStyle)
{
    StyledText::const_iterator current = inText.begin() + inBegin;
    StyledText::const_iterator end = inText.begin() + inEnd;
    for (; current != end; ++current)
		TEST(*current->style == inDesiredStyle);
}

void test_TStyleSheet(void)
{
    // Create and get our stylesheet.
	TDummyArgumentList dummy_args;
	gStyleSheetManager.AddStyleSheet(dummy_args);
	TEST(!dummy_args.HasMoreArguments());
	TStyleSheet *style1 = gStyleSheetManager.Find("S1");
	TEST(style1 != NULL);

    // Set up a style.
    StyledText text1 =
		style1->MakeStyledText(" foo |bar| ^baz^ @wub@ || "
							   "|foo@bar@| \\n\\t\\^\\@\\|\\\\");
    TEST(*text1.GetText() == L" foo bar baz wub  foobar \n\t^@|\\");

    // Build something to compare it against.
    Typography::Style base_style("Times", 12);
	std::list<std::string> backups;
	backups.push_back("Standard Symbols L");
	backups.push_back("Dingbats");
	base_style.SetBackupFamilies(backups);	
	base_style.SetFaceStyle(kShadowFaceStyle);
    base_style.SetLeading(-1);
	base_style.SetShadowOffset(2);
    base_style.SetColor(Color(0xFF, 0xFF, 0xFF, 0x00));
    base_style.SetShadowColor(Color(0x00, 0x00, 0x00, 0x80));
    Typography::Style highlight_style(base_style);
    highlight_style.SetColor(Color(0x00, 0xFF, 0x00, 0x00));
    highlight_style.SetShadowColor(Color(0x00, 0xFF, 0x00, 0x80));
        
    // Make sure each of our ranges is correct.
    test_style(text1, 0, 5, base_style);
    test_style(text1, 5, 8,
			   Typography::Style(base_style).ToggleFaceStyle(kItalicFaceStyle));
    test_style(text1, 8, 9, base_style);
    test_style(text1, 9, 10, highlight_style);
    test_style(text1, 12, 13, base_style);
    test_style(text1, 13, 16,
			   Typography::Style(base_style).ToggleFaceStyle(kBoldFaceStyle));
    test_style(text1, 16, 18, base_style);
    test_style(text1, 18, 21,
			   Typography::Style(base_style).ToggleFaceStyle(kItalicFaceStyle));
    test_style(text1, 21, 24,
			   Typography::Style(base_style).ToggleFaceStyle(kBoldItalicFaceStyle));
}
