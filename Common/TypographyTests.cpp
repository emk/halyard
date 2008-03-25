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

#include "CommonHeaders.h"

#include <string.h>
#include <stdlib.h>

#include "ImlUnit.h"
#include "Typography.h"

using namespace Typography;

extern void test_Typography (void);


//=========================================================================
//	Style Tests
//=========================================================================

static void test_Typography_Style (void)
{
	// Test constructor
	Style s1("Nimbus Roman No9 L", 14);
	TEST(s1.GetFamily() == "Nimbus Roman No9 L");
	TEST(s1.GetFaceStyle() == kRegularFaceStyle);
	TEST(s1.GetSize() == 14);
	TEST(s1.GetLeading() == 0);
	TEST(s1.GetShadowOffset() == 1);
	TEST(s1.GetColor() == Color(0, 0, 0));
	TEST(s1.GetShadowColor() == Color(255, 255, 255));

	// Test assignment
	Style s2("Times", 12);
	s2 = s1;
	TEST(s2.GetFamily() == "Nimbus Roman No9 L");
	TEST(s2.GetFaceStyle() == kRegularFaceStyle);
	TEST(s2.GetSize() == 14);
	TEST(s2.GetLeading() == 0);
	TEST(s2.GetShadowOffset() == 1);
	TEST(s2.GetColor() == Color(0, 0, 0));
	TEST(s2.GetShadowColor() == Color(255, 255, 255));
	
	// Test copy constructor
	Style s3(s1);
	TEST(s3.GetFamily() == "Nimbus Roman No9 L");
	TEST(s3.GetFaceStyle() == kRegularFaceStyle);
	TEST(s3.GetSize() == 14);
	TEST(s3.GetLeading() == 0);
	TEST(s3.GetShadowOffset() == 1);
	TEST(s3.GetColor() == Color(0, 0, 0));
	TEST(s3.GetShadowColor() == Color(255, 255, 255));

	// Test setting of properties
	s1.SetFamily("Times");
	TEST(s1.GetFamily() == "Times");
	TEST(s2.GetFamily() == "Nimbus Roman No9 L");
	s1.SetFaceStyle(kBoldFaceStyle);
	TEST(s1.GetFaceStyle() == kBoldFaceStyle);
	TEST(s2.GetFaceStyle() == kRegularFaceStyle);
	s1.SetSize(18);
	TEST(s1.GetSize() == 18);
	TEST(s2.GetSize() == 14);
	s1.SetLeading(2);
	TEST(s1.GetLeading() == 2);
	TEST(s2.GetLeading() == 0);
	s1.SetShadowOffset(2);
	TEST(s1.GetShadowOffset() == 2);
	TEST(s2.GetShadowOffset() == 1);
	s1.SetColor(Color(32, 32, 32));
	TEST(s1.GetColor() == Color(32, 32, 32));
	TEST(s2.GetColor() == Color(0, 0, 0));
	s1.SetShadowColor(Color(64, 64, 64));
	TEST(s1.GetShadowColor() == Color(64, 64, 64));
	TEST(s2.GetShadowColor() == Color(255, 255, 255));

	// Test fetching of faces
	TEST(s1.GetFace()->GetSize() == 18);
	TEST(s2.GetFace()->GetSize() == 14);
	TEST(s3.GetFace()->GetSize() == 14);
	s1.SetSize(12);
	TEST(s1.GetFace()->GetSize() == 12);

	// Test drawing properties
	s1.SetFaceStyle(kUnderlineFaceStyle);
	TEST(s1.GetIsUnderlined());
	TEST(!s1.GetIsShadowed());
	s1.SetFaceStyle(kShadowFaceStyle);
	TEST(!s1.GetIsUnderlined());
	TEST(s1.GetIsShadowed());

	// Test toggling of FaceStyle flags.
	s1.SetFaceStyle(kBoldItalicFaceStyle | kShadowFaceStyle);
	s1.ToggleFaceStyle(kBoldFaceStyle | kUnderlineFaceStyle);
	TEST(s1.GetFaceStyle() ==
		 (kItalicFaceStyle | kUnderlineFaceStyle | kShadowFaceStyle));
}


//=========================================================================
//	Style Information Tests
//=========================================================================

static void test_Typography_StyledText (void)
{
	Style base("Nimbus Roman No9 L", 12);
	StyledText info(base);
	info.AppendText(L"xy");
	info.ChangeStyle(Style(base).SetFaceStyle(kItalicFaceStyle));
	info.AppendText(L"xy");
	info.ChangeStyle(Style(base).SetColor(Color(255, 0, 0)));
	info.AppendText(L"xy");
	info.ChangeStyle(Style(base).SetColor(Color(0, 255, 0)));
	info.ChangeStyle(Style(base).SetColor(Color(0, 0, 255)));
	info.AppendText(L'x');
	info.AppendText(L'y');
	info.EndConstruction();
	TEST(*info.GetText() == L"xyxyxyxy");

	// Make sure we have correct style information.
	StyledText::const_iterator iter = info.begin();
	TEST(iter->style->GetSize() == 12);                         // 0
	TEST(iter->style->GetFaceStyle() == kRegularFaceStyle);     // 0
	TEST((++iter)->style->GetFaceStyle() == kRegularFaceStyle); // 1
	StyledText::const_iterator iter2 = iter;                    // COPY AT 1
	TEST((++iter)->style->GetFaceStyle() == kItalicFaceStyle);  // 2
	TEST((++iter)->style->GetFaceStyle() == kItalicFaceStyle);  // 3
	TEST((++iter)->style->GetFaceStyle() == kRegularFaceStyle); // 4
	TEST(iter->style->GetColor() == Color(255, 0, 0));          // 4
	TEST((++iter)->style->GetColor() == Color(255, 0, 0));      // 5
	TEST((++iter)->style->GetColor() == Color(0, 0, 255));      // 6
	TEST((++iter)->style->GetColor() == Color(0, 0, 255));      // 7
	TEST(++iter == info.end());                                 // 8
	
	// Test iterator copy.
	StyledText::const_iterator iter3 = iter2;                   // COPY AT 1
	TEST(iter2 == iter3);                                       // 1 == 1
	TEST(iter2->style->GetFaceStyle() == kRegularFaceStyle);    // 1
	TEST((++iter2)->style->GetFaceStyle() == kItalicFaceStyle); // 2	
	TEST(iter2 != iter3);                                       // 1 != 2
}


//=========================================================================
//	LineSegment Tests
//=========================================================================

static const StyledText *make_text(const std::wstring &inString)
{
	StyledText *styledText = new StyledText(Style("Nimbus Roman No9 L", 12));
	styledText->AppendText(inString);
	styledText->EndConstruction();
	return styledText;
}

static LineSegment make_test_seg(const StyledText *inText,
								 int inBegin, int inEnd,
								 bool inIsLineBreak = false,
								 bool inDiscardAtEndOfLine = false,
								 bool inNeedsHyphenAtEndOfLine = false)
{
	StyledText::const_iterator begin = inText->begin() + inBegin;
	StyledText::const_iterator end = inText->begin() + inEnd;
	return LineSegment(begin, end, inIsLineBreak, inDiscardAtEndOfLine,
					   inNeedsHyphenAtEndOfLine);
}

#define TEST_SEGMENT(seg,text,from,to,b1,b2,b3) \
    TEST(seg == make_test_seg(text, from, to, b1, b2, b3))

static void test_Typography_LineSegment (void) 
{
	// Simple words & whitespace.
	std::wstring text1 = L"abc def  ghi";
	const StyledText *styledtext1 = make_text(text1);
	LineSegmentIterator iter1(*styledtext1);
	LineSegment seg1;
	TEST(iter1.NextElement(&seg1));
	TEST_SEGMENT(seg1, styledtext1, 0, 3, false, false, false);
	TEST(iter1.NextElement(&seg1));
	TEST_SEGMENT(seg1, styledtext1, 3, 4, false, true,  false);
	TEST(iter1.NextElement(&seg1));
	TEST_SEGMENT(seg1, styledtext1, 4, 7, false, false, false);
	TEST(iter1.NextElement(&seg1));
	TEST_SEGMENT(seg1, styledtext1, 7, 9, false, true,  false);
	TEST(iter1.NextElement(&seg1));
	TEST_SEGMENT(seg1, styledtext1, 9, 12, false, false,  false);
	TEST(!iter1.NextElement(&seg1));
	delete styledtext1;

	// Basic hyphenation.
	std::wstring text2 = L"abc-def- ghi---";
	const StyledText *styledtext2 = make_text(text2);
	LineSegmentIterator iter2(*styledtext2);
	LineSegment seg2;
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, styledtext2, 0, 4, false, false, false);
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, styledtext2, 4, 8, false, false, false);
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, styledtext2, 8, 9, false, true, false);
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, styledtext2, 9, 13, false, false, false);
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, styledtext2, 13, 14, false, false, false);
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, styledtext2, 14, 15, false, false, false);
	TEST(!iter2.NextElement(&seg2));
	delete styledtext2;

	// Explicit newlines.
	std::wstring text3 = L" abc\ndef \n\n ghi";
	const StyledText *styledtext3 = make_text(text3);
	LineSegmentIterator iter3(*styledtext3);
	LineSegment seg3;
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, styledtext3, 0, 1, false, true, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, styledtext3, 1, 4, false, false, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, styledtext3, 4, 5, true, false, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, styledtext3, 5, 8, false, false, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, styledtext3, 8, 9, false, true, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, styledtext3, 9, 10, true, false, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, styledtext3, 10, 11, true, false, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, styledtext3, 11, 12, false, true, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, styledtext3, 12, 15, false, false, false);
	TEST(!iter3.NextElement(&seg3));
	delete styledtext3;

	// Soft hyphens.
	std::wstring text4 = L"abcXdefX ghi\nXjklXXX\n X ";
	for (size_t i = 0; i < text4.length(); i++)
	{
		// Convert 'X' to a soft hyphen.
		if (text4[i] == 'X')
			text4[i] = kSoftHyphen;
		else
			text4[i] = text4[i];
	}
	const StyledText *styledtext4 = make_text(text4);
	LineSegmentIterator iter4(*styledtext4);
	LineSegment seg4;
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, styledtext4, 0, 3, false, false, true);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, styledtext4, 4, 7, false, false, true);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, styledtext4, 8, 9, false, true, false);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, styledtext4, 9, 12, false, false, false);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, styledtext4, 12, 13, true, false, false);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, styledtext4, 14, 17, false, false, true);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, styledtext4, 20, 21, true, false, false);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, styledtext4, 21, 22, false, true, false);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, styledtext4, 23, 24, false, true, false);
	TEST(!iter4.NextElement(&seg4));
	delete styledtext4;

	// TODO - Non-breaking space.
	// TODO - mdash, ndash.
}


//=========================================================================
//	GenericTextRenderingEngine Tests
//=========================================================================
//	We subclass the GenericTextRenderingEngine with a rendering engine
//	based on n-column string output.  This allows us to test about 90%
//	of the functionality of the GenericTextRenderingEngine without fooling
//	around with bitmaps.  We don't test the 'inPrevious' argument of
//	'MeasureSegment', which is used for intersegment-kerning.

class TestTextRenderingEngine : public GenericTextRenderingEngine
{
private:
	std::basic_string<wchar_t> mRenderedText;

public:
	TestTextRenderingEngine(const StyledText &inText,
							Distance inLineLength,
							Justification inJustification)
		: GenericTextRenderingEngine(inText, inLineLength,
									 inJustification) {}
	
	void Test(const wchar_t *result);
	
protected:
	virtual Distance MeasureSegment(LineSegment *inPrevious,
									LineSegment *inSegment,
									bool inAtEndOfLine);
	virtual void ExtractOneLine(LineSegment *ioRemaining,
								LineSegment *outExtracted);
	virtual void RenderLine(std::deque<LineSegment> *inLine,
							Distance inHorizontalOffset);
};

// Since nobody seems to print wchar_t strings correctly,
// convert them to ASCII.
static std::string to_ascii (const std::wstring &input)
{
	std::string result;
	for (std::basic_string<wchar_t>::const_iterator i = input.begin();
		 i < input.end(); i++)
	{
		wchar_t c = *i;
		if (c < 128)
			result += (char) c;
		else
			result += '.';
	}
	return result;
}

void TestTextRenderingEngine::Test (const wchar_t *result)
{
	RenderText();
	if (mRenderedText != result)
	{
		// Many debuggers can't print whcar_t* and/or string<wchar_t>
		// data correctly, so display some more data about the test failure.
		std::cout << "\n--- Result:\n" << to_ascii(mRenderedText).c_str()
			      << "--- Expected:\n" << to_ascii(result).c_str() << "---";
	}
	TEST(mRenderedText == result);
}

Distance TestTextRenderingEngine::MeasureSegment(LineSegment *inPrevious,
												 LineSegment *inSegment,
												 bool inAtEndOfLine)
{
	TEST(inSegment != NULL);
	Distance result = inSegment->end - inSegment->begin;
	if (inAtEndOfLine && inSegment->needsHyphenAtEndOfLine)
		++result;
	return result;
}

void TestTextRenderingEngine::ExtractOneLine(LineSegment *ioRemaining,
											 LineSegment *outExtracted)
{
	TEST(ioRemaining != NULL);
	TEST(ioRemaining->isLineBreak == false);
	TEST(ioRemaining->discardAtEndOfLine == false);
	TEST(outExtracted != NULL);

	StyledText::const_iterator new_end =
		ioRemaining->begin + (GetLineLength() - 1);
	outExtracted->SetLineSegment(ioRemaining->begin, new_end,
								 false, false, true);
	TEST(outExtracted->end != ioRemaining->end);
	ioRemaining->begin = outExtracted->end;	
}

void TestTextRenderingEngine::RenderLine(std::deque<LineSegment> *inLine,
										 Distance inHorizontalOffset)
{
	for (Distance i = 0; i < inHorizontalOffset; i++)
		mRenderedText += L' ';
	mRenderedText += L'|';
	
	// Render each segment.
	for (std::deque<LineSegment>::iterator iter = inLine->begin();
		 iter < inLine->end(); iter++)
	{
		LineSegment seg = *iter;
		for (StyledText::const_iterator iter = seg.begin;
			 iter != seg.end; ++iter)
			mRenderedText += iter->value;
	}
	
	// Add a trailing hyphen (if the final segment wants one).
	if (!inLine->empty() && inLine->back().needsHyphenAtEndOfLine)
		mRenderedText += L"-";
	
	mRenderedText += L"|\n";
}

void rendering_test(const wchar_t *in, Distance width,
					Justification justification, const wchar_t *out)
{
	// Insert various special characters which we can't reliably
	// escape with some C compilers.
	std::wstring s = in;
	for (std::wstring::iterator i = s.begin(); i < s.end(); i++)
		if (*i == 'X')
			*i = kSoftHyphen;

	// Set up the rest of our parameters and call the engine.
	const StyledText *text = make_text(s);
	TestTextRenderingEngine e(*text, width, justification);
	e.Test(out);
	delete text;
}

#define RTEST_L(in,width,out) rendering_test(in,width,kLeftJustification,out)
#define RTEST_C(in,width,out) rendering_test(in,width,kCenterJustification,out)
#define RTEST_R(in,width,out) rendering_test(in,width,kRightJustification,out)

static void test_Typography_GenericTextRenderingEngine (void) 
{
	// Simple words.
	RTEST_L(L"abc defg", 5, L"|abc|\n|defg|\n");
	RTEST_C(L"abc defg", 5, L" |abc|\n|defg|\n");
	RTEST_R(L"abc defg", 5, L"  |abc|\n |defg|\n");

	// Simple hyphenation.
	RTEST_L(L"abc-def", 5, L"|abc-|\n|def|\n");
	RTEST_C(L"abc-def", 5, L"|abc-|\n |def|\n");
	RTEST_R(L"abc-def", 5, L" |abc-|\n  |def|\n");
	RTEST_L(L"abc---def", 5, L"|abc--|\n|-def|\n");
	RTEST_C(L"abc---def", 5, L"|abc--|\n|-def|\n");
	RTEST_R(L"abc---def", 5, L"|abc--|\n |-def|\n");

	// Overlong words with forced breaks.
	RTEST_L(L"abcdefg", 5, L"|abcd-|\n|efg|\n");
	RTEST_C(L"abcdefg", 5, L"|abcd-|\n |efg|\n");
	RTEST_R(L"abcdefg", 5, L"|abcd-|\n  |efg|\n");
	RTEST_L(L"x abcdefg", 5, L"|x|\n|abcd-|\n|efg|\n");
	RTEST_C(L"x abcdefg", 5, L"  |x|\n|abcd-|\n |efg|\n");
	RTEST_R(L"x abcdefg", 5, L"    |x|\n|abcd-|\n  |efg|\n");

	// Soft hyphenation.
	RTEST_L(L"abcXdefXXg", 5, L"|abc-|\n|defg|\n");
	RTEST_C(L"abcXdefXXg", 5, L"|abc-|\n|defg|\n");
	RTEST_R(L"abcXdefXXg", 5, L" |abc-|\n |defg|\n");
	RTEST_L(L"abcXdefgX", 5, L"|abc-|\n|defg-|\n");
	RTEST_C(L"abcXdefgX", 5, L"|abc-|\n|defg-|\n");
	RTEST_R(L"abcXdefgX", 5, L" |abc-|\n|defg-|\n");
	RTEST_L(L"abcXdefgX ", 5, L"|abc-|\n|defg-|\n");
	RTEST_C(L"abcXdefgX ", 5, L"|abc-|\n|defg-|\n");
	RTEST_R(L"abcXdefgX ", 5, L" |abc-|\n|defg-|\n");
	RTEST_L(L"Xabcdefg", 5, L"|abcd-|\n|efg|\n");
	RTEST_C(L"Xabcdefg", 5, L"|abcd-|\n |efg|\n");
	RTEST_R(L"Xabcdefg", 5, L"|abcd-|\n  |efg|\n");
	RTEST_L(L"a bcXdef", 5, L"|a bc-|\n|def|\n");
	RTEST_C(L"a bcXdef", 5, L"|a bc-|\n |def|\n");
	RTEST_R(L"a bcXdef", 5, L"|a bc-|\n  |def|\n");
	RTEST_L(L"a bXdef", 5, L"|a b-|\n|def|\n");
	RTEST_C(L"a bXdef", 5, L"|a b-|\n |def|\n");
	RTEST_R(L"a bXdef", 5, L" |a b-|\n  |def|\n");
	RTEST_L(L"a bcdXef", 5, L"|a|\n|bcdef|\n");
	RTEST_C(L"a bcdXef", 5, L"  |a|\n|bcdef|\n");
	RTEST_R(L"a bcdXef", 5, L"    |a|\n|bcdef|\n");

	// Explicit newlines.
	RTEST_L(L"abc\ndef", 5, L"|abc|\n|def|\n");
	RTEST_C(L"abc\ndef", 5, L" |abc|\n |def|\n");
	RTEST_R(L"abc\ndef", 5, L"  |abc|\n  |def|\n");
	RTEST_L(L"abc \n def", 5, L"|abc|\n| def|\n");
	RTEST_C(L"abc \n def", 5, L" |abc|\n| def|\n");
	RTEST_R(L"abc \n def", 5, L"  |abc|\n | def|\n");

	// Other random cases.
	RTEST_L(L"abc   def  ", 5, L"|abc|\n|def|\n");
	RTEST_C(L"abc   def  ", 5, L" |abc|\n |def|\n");
	RTEST_R(L"abc   def  ", 5, L"  |abc|\n  |def|\n");
	RTEST_L(L"abc   def   ", 5, L"|abc|\n|def|\n||\n");
	RTEST_C(L"abc   def   ", 5, L" |abc|\n |def|\n  ||\n");
	RTEST_R(L"abc   def   ", 5, L"  |abc|\n  |def|\n     ||\n");
	RTEST_L(L"", 5, L"||\n");
	RTEST_C(L"", 5, L"  ||\n");
	RTEST_R(L"", 5, L"     ||\n");
	RTEST_L(L"\n\n", 2, L"||\n||\n||\n");
	RTEST_C(L"\n\n", 2, L" ||\n ||\n ||\n");
	RTEST_R(L"\n\n", 2, L"  ||\n  ||\n  ||\n");

	// TODO - Test tabs.
}


//=========================================================================
//	GenericTextRenderingEngine Tests
//=========================================================================

static void test_Typography_FamilyDatabase (void)
{
	// Delete the cache file.
	try
	{
		FileSystem::GetFontFilePath("cache.dat").RemoveFile();
	}
	catch (FileSystem::Error)
	{
		// Ignore errors.
	}

	// Create a database directly.
	FamilyDatabase db1;
	db1.ReadFromFontDirectory();

	// Create a second database using the cache.
	FamilyDatabase db2;
	db2.ReadFromFontDirectory();
	
	// Try looking up some fonts.  (These tests rely on some knowledge
	// about the available fonts, and will need to be updated if the fonts
	// in the Font directory change.)
	Face f1 = db1.GetFace("Times", kRegularFaceStyle, 12);
	TEST(f1.GetFamilyName() == "Times");
	TEST(f1.GetStyleName() == "Regular");
	TEST(f1.GetSize() == 12);
	Face f2 = db1.GetFace("Times", kBoldItalicFaceStyle, 18);
	TEST(f2.GetFamilyName() == "Times");
	TEST(f2.GetStyleName() == "Bold Italic");
	TEST(f2.GetSize() == 18);

	// For now, we don't allow scaling of bitmapped fonts.
	TEST_EXCEPTION(db1.GetFace("Times", kBoldItalicFaceStyle, 17), Error);

	// A scalable font.
	Face f3 = db1.GetFace("Nimbus Roman No9 L", kRegularFaceStyle, 12);
	TEST(f3.GetFamilyName() == "Nimbus Roman No9 L");
	TEST(f3.GetStyleName() == "Regular");
	TEST(f3.GetSize() == 12);
	Face f4 = db1.GetFace("Nimbus Roman No9 L", kRegularFaceStyle, 15);
	TEST(f4.GetFamilyName() == "Nimbus Roman No9 L");
	TEST(f4.GetStyleName() == "Regular");
	TEST(f4.GetSize() == 15);

	// Style substitution.
	Face f5 = db1.GetFace("Standard Symbols L", kRegularFaceStyle, 12);
	TEST(f5.GetFamilyName() == "Standard Symbols L");
	TEST(f5.GetStyleName() == "Regular");
	TEST(f5.GetSize() == 12);
	Face f6 = db1.GetFace("Standard Symbols L", kBoldFaceStyle, 12);
	TEST(f6.GetStyleName() == "Regular");
	Face f7 = db1.GetFace("Standard Symbols L", kItalicFaceStyle, 12);
	TEST(f7.GetStyleName() == "Regular");
	Face f8 = db1.GetFace("Standard Symbols L", kBoldItalicFaceStyle, 12);
	TEST(f8.GetStyleName() == "Regular");

	// Test serialization and deserialization.
	std::ostringstream outstream;
	db1.WriteToCache(outstream);
	std::string outstring = outstream.str();
	std::istringstream instream(outstring.c_str());
	FamilyDatabase db3;
	db3.ReadFromCache(instream);
	std::ostringstream outstream2;
	db3.WriteToCache(outstream2);
	TEST(outstring == outstream2.str());
}


//=========================================================================
//	Test Driver
//=========================================================================

void test_Typography (void)
{
	test_Typography_Style();
	test_Typography_StyledText();
	test_Typography_LineSegment();
	test_Typography_GenericTextRenderingEngine();
	test_Typography_FamilyDatabase();
}
