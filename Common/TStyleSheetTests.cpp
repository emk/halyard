// -*- Mode: C++; tab-width: 4; -*-

#include "ImlUnit.h"
#include "TStyleSheet.h"
#include "TParser.h"

USING_NAMESPACE_FIVEL

using namespace Typography;

extern void test_TStyleSheet(void);

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
    // Install support for top-level forms of type "defstyle".
    TStyleSheetManager style_manager;
    TParser::RegisterIndexManager("defstyle", &style_manager);

    // Parse our index file and get our style sheet.
    gIndexFileManager.NewIndex("defstyle");
	TBNode *node = style_manager.Find("S1");
	TEST(node != NULL);

    // Set up a style.
    TStyleSheet *style1 = dynamic_cast<TStyleSheet*>(node);
    StyledText text1 = style1->MakeStyledText(" foo |bar| ^baz^ @wub@ || "
											  "|foo@bar@|");
    TEST(*text1.GetText() == L"foo bar baz wub  foobar");

    // Build something to compare it against.
    Typography::Style base_style("Times", 12);
	base_style.SetFaceStyle(kShadowFaceStyle);
    base_style.SetLeading(-1);
	base_style.SetShadowOffset(2);
    base_style.SetColor(Color(0xFF, 0xFF, 0xFF, 0x00));
    base_style.SetShadowColor(Color(0x00, 0x00, 0x00, 0x80));
    Typography::Style highlight_style(base_style);
    highlight_style.SetColor(Color(0x00, 0xFF, 0x00, 0x00));
    highlight_style.SetShadowColor(Color(0x00, 0xFF, 0x00, 0x80));
        
    // Make sure each of our ranges is correct.
    test_style(text1, 0, 4, base_style);
    test_style(text1, 4, 7,
			   Typography::Style(base_style).ToggleFaceStyle(kItalicFaceStyle));
    test_style(text1, 7, 8, base_style);
    test_style(text1, 8, 11, highlight_style);
    test_style(text1, 11, 12, base_style);
    test_style(text1, 12, 15,
			   Typography::Style(base_style).ToggleFaceStyle(kBoldFaceStyle));
    test_style(text1, 15, 17, base_style);
    test_style(text1, 17, 20,
			   Typography::Style(base_style).ToggleFaceStyle(kItalicFaceStyle));
    test_style(text1, 20, 23,
			   Typography::Style(base_style).ToggleFaceStyle(kBoldItalicFaceStyle));
}
