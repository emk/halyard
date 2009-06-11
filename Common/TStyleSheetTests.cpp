// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#include "ImlUnit.h"
#include "TStyleSheet.h"
#include "TPrimitives.h"

using namespace Halyard;

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
    // Create and get our stylesheet.
    TValueList arg_values;
    arg_values.push_back(TSymbol("S1"));
    arg_values.push_back("Times");
    arg_values.push_back(12);
    arg_values.push_back(TSymbol("r"));
    arg_values.push_back(TSymbol("right"));
    arg_values.push_back(GraphicsTools::Color(0xFF, 0xFF, 0xFF, 0x00));
    arg_values.push_back(GraphicsTools::Color(0x00, 0xff, 0x00, 0x00));
    arg_values.push_back(-1);
    arg_values.push_back(2);
    arg_values.push_back(GraphicsTools::Color(0x00, 0x00, 0x00, 0x80));
    arg_values.push_back(GraphicsTools::Color(0x00, 0xFF, 0x00, 0x80));
    TArgumentList args(arg_values);
    gStyleSheetManager.AddStyleSheet(args);
    TEST(!args.HasMoreArguments());
    TStyleSheet *style1 = gStyleSheetManager.Find("S1");
    TEST(style1 != NULL);

    // Set up a style.
    StyledText text1 =
        style1->MakeStyledText(" foo <u>bar</u> <h>baz</h> <b>wub</b> <i></i> "
                               "<i>foo<b>bar</b></i> &lt;&gt;&amp;");
    TEST(*text1.GetText() == L" foo bar baz wub  foobar <>&");

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
               Typography::Style(base_style).ToggleFaceStyle(kUnderlineFaceStyle));
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
