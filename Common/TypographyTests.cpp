// -*- Mode: C++; tab-width: 4; -*-

#include <string>
#include <iostream>
#include <strstream>

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
	TEST(s1.GetColor() == Color(0, 0, 0));
	TEST(s1.GetShadowColor() == Color(255, 255, 255));

	// Test assignment
	Style s2("Times", 12);
	s2 = s1;
	TEST(s2.GetFamily() == "Nimbus Roman No9 L");
	TEST(s2.GetFaceStyle() == kRegularFaceStyle);
	TEST(s2.GetSize() == 14);
	TEST(s2.GetColor() == Color(0, 0, 0));
	TEST(s2.GetShadowColor() == Color(255, 255, 255));
	
	// Test copy constructor
	Style s3(s1);
	TEST(s3.GetFamily() == "Nimbus Roman No9 L");
	TEST(s3.GetFaceStyle() == kRegularFaceStyle);
	TEST(s3.GetSize() == 14);
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
}


//=========================================================================
//	Style Information Tests
//=========================================================================

static void test_Typography_StyleInformation (void)
{
	Style base("Nimbus Roman No9 L", 12);
	StyleInformation info(base);
	info.ChangeStyleAt(2, Style(base).SetFaceStyle(kItalicFaceStyle));
	info.ChangeStyleAt(4, Style(base).SetColor(Color(255, 0, 0)));
	info.ChangeStyleAt(6, Style(base).SetColor(Color(0, 255, 0)));
	info.ChangeStyleAt(6, Style(base).SetColor(Color(0, 0, 255)));
	info.EndStyleAt(8);

	// Make sure we have correct style information.
	StyleInformation::const_iterator iter = info.begin();
	TEST(iter->GetSize() == 12);                         // 0
	TEST(iter->GetFaceStyle() == kRegularFaceStyle);     // 0
	TEST((++iter)->GetFaceStyle() == kRegularFaceStyle); // 1
	StyleInformation::const_iterator iter2 = iter;       // COPY AT 1
	TEST((++iter)->GetFaceStyle() == kItalicFaceStyle);  // 2
	TEST((++iter)->GetFaceStyle() == kItalicFaceStyle);  // 3
	TEST((++iter)->GetFaceStyle() == kRegularFaceStyle); // 4
	TEST(iter->GetColor() == Color(255, 0, 0));          // 4
	TEST((++iter)->GetColor() == Color(255, 0, 0));      // 5
	TEST((++iter)->GetColor() == Color(0, 0, 255));      // 6
	TEST((++iter)->GetColor() == Color(0, 0, 255));      // 7
	TEST(++iter == info.end());                          // 8
	
	// Test iterator copy.
	StyleInformation::const_iterator iter3 = iter2;      // COPY AT 1
	TEST(iter2 == iter3);                                // 1 == 1
	TEST(iter2->GetFaceStyle() == kRegularFaceStyle);    // 1
	TEST((++iter2)->GetFaceStyle() == kItalicFaceStyle); // 2	
	TEST(iter2 != iter3);                                // 1 != 2
}


//=========================================================================
//	LineSegment Tests
//=========================================================================

static void test_Typography_LineSegment (void) 
{
	// Simple words & whitespace.
	const wchar_t *text1 = L"abc def  ghi";
	LineSegmentIterator iter1(text1, text1 + wcslen(text1));
	LineSegment seg1;
	TEST(iter1.NextElement(&seg1));
	TEST(seg1 == LineSegment(text1, text1 + 3));
	TEST(iter1.NextElement(&seg1));
	TEST(seg1 == LineSegment(text1 + 3, text1 + 4, false, true));
	TEST(iter1.NextElement(&seg1));
	TEST(seg1 == LineSegment(text1 + 4, text1 + 7));
	TEST(iter1.NextElement(&seg1));
	TEST(seg1 == LineSegment(text1 + 7, text1 + 9, false, true));
	TEST(iter1.NextElement(&seg1));
	TEST(seg1 == LineSegment(text1 + 9, text1 + 12));
	TEST(!iter1.NextElement(&seg1));

	// Basic hyphenation.
	const wchar_t *text2 = L"abc-def- ghi---";
	LineSegmentIterator iter2(text2, text2 + wcslen(text2));
	LineSegment seg2;
	TEST(iter2.NextElement(&seg2));
	TEST(seg2 == LineSegment(text2, text2 + 4));
	TEST(iter2.NextElement(&seg2));
	TEST(seg2 == LineSegment(text2 + 4, text2 + 8));
	TEST(iter2.NextElement(&seg2));
	TEST(seg2 == LineSegment(text2 + 8, text2 + 9, false, true));
	TEST(iter2.NextElement(&seg2));
	TEST(seg2 == LineSegment(text2 + 9, text2 + 13));
	TEST(iter2.NextElement(&seg2));
	TEST(seg2 == LineSegment(text2 + 13, text2 + 14));
	TEST(iter2.NextElement(&seg2));
	TEST(seg2 == LineSegment(text2 + 14, text2 + 15));
	TEST(!iter2.NextElement(&seg2));

	// Explicit newlines.
	const wchar_t *text3 = L" abc\ndef \n\n ghi";
	LineSegmentIterator iter3(text3, text3 + wcslen(text3));
	LineSegment seg3;
	TEST(iter3.NextElement(&seg3));
	TEST(seg3 == LineSegment(text3, text3 + 1, false, true));
	TEST(iter3.NextElement(&seg3));
	TEST(seg3 == LineSegment(text3 + 1, text3 + 4));
	TEST(iter3.NextElement(&seg3));
	TEST(seg3 == LineSegment(text3 + 4, text3 + 5, true));
	TEST(iter3.NextElement(&seg3));
	TEST(seg3 == LineSegment(text3 + 5, text3 + 8));
	TEST(iter3.NextElement(&seg3));
	TEST(seg3 == LineSegment(text3 + 8, text3 + 9, false, true));
	TEST(iter3.NextElement(&seg3));
	TEST(seg3 == LineSegment(text3 + 9, text3 + 10, true));
	TEST(iter3.NextElement(&seg3));
	TEST(seg3 == LineSegment(text3 + 10, text3 + 11, true));
	TEST(iter3.NextElement(&seg3));
	TEST(seg3 == LineSegment(text3 + 11, text3 + 12, false, true));
	TEST(iter3.NextElement(&seg3));
	TEST(seg3 == LineSegment(text3 + 12, text3 + 15));
	TEST(!iter3.NextElement(&seg3));

	// Soft hyphens.
	const wchar_t *raw_text4 = L"abcXdefX ghi\nXjklXXX\n X ";
	size_t text4_len = wcslen(raw_text4) + 1;
	wchar_t *text4 = (wchar_t*) malloc(sizeof(wchar_t) * text4_len);
	TEST(text4);
	for (size_t i = 0; i < text4_len; i++)
	{
		// Convert 'X' to a soft hyphen.
		if (raw_text4[i] == 'X')
			text4[i] = kSoftHyphen;
		else
			text4[i] = raw_text4[i];
	}
	LineSegmentIterator iter4(text4, text4 + wcslen(text4));
	LineSegment seg4;
	TEST(iter4.NextElement(&seg4));
	TEST(seg4 == LineSegment(text4, text4 + 3, false, false, true));
	TEST(iter4.NextElement(&seg4));
	TEST(seg4 == LineSegment(text4 + 4, text4 + 7, false, false, true));
	TEST(iter4.NextElement(&seg4));
	TEST(seg4 == LineSegment(text4 + 8, text4 + 9, false, true));
	TEST(iter4.NextElement(&seg4));
	TEST(seg4 == LineSegment(text4 + 9, text4 + 12));
	TEST(iter4.NextElement(&seg4));
	TEST(seg4 == LineSegment(text4 + 12, text4 + 13, true));
	TEST(iter4.NextElement(&seg4));
	TEST(seg4 == LineSegment(text4 + 14, text4 + 17, false, false, true));
	TEST(iter4.NextElement(&seg4));
	TEST(seg4 == LineSegment(text4 + 20, text4 + 21, true));
	TEST(iter4.NextElement(&seg4));
	TEST(seg4 == LineSegment(text4 + 21, text4 + 22, false, true));
	TEST(iter4.NextElement(&seg4));
	TEST(seg4 == LineSegment(text4 + 23, text4 + 24, false, true));
	TEST(!iter4.NextElement(&seg4));
	free(text4);

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
	TestTextRenderingEngine(const wchar_t *inTextBegin,
							const wchar_t *inTextEnd,
							Distance inLineLength,
							Justification inJustification)
		: GenericTextRenderingEngine(inTextBegin, inTextEnd, inLineLength,
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
static std::string to_ascii (const std::basic_string<wchar_t> &input)
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
		result++;
	return result;
}

void TestTextRenderingEngine::ExtractOneLine(LineSegment *ioRemaining,
											 LineSegment *outExtracted)
{
	TEST(ioRemaining != NULL);
	TEST(ioRemaining->isLineBreak == false);
	TEST(ioRemaining->discardAtEndOfLine == false);
	TEST(outExtracted != NULL);

	outExtracted->SetLineSegment(ioRemaining->begin,
								 ioRemaining->begin + (GetLineLength() - 1),
								 false, false, true);
	TEST(outExtracted->end < ioRemaining->end);
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
		for (const wchar_t *c = seg.begin; c < seg.end; c++)
			mRenderedText += *c;
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
	std::basic_string<wchar_t> s = in;
	for (std::basic_string<wchar_t>::iterator i = s.begin(); i < s.end(); i++)
		if (*i == 'X')
			*i = kSoftHyphen;

	const wchar_t *s_begin = s.c_str();
	const wchar_t *s_end = s_begin + wcslen(s_begin);
	TestTextRenderingEngine e(s_begin, s_end, width, justification);
	e.Test(out);
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

static std::string get_string(std::ostrstream &stream)
{
	// Go through the foolish new rigamarole for extracting a string.
	// We must unfreeze the stream before we exit this function, or
	// we'll leak memory.
	stream.freeze(1);
	try
	{
		std::string str(stream.str(), stream.pcount());
		stream.freeze(0);
		return str;
	}
	catch (...)
	{
		stream.freeze(0);
		throw;
	}
	
	ASSERT(false);
	return std::string("");
}

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
	std::ostrstream outstream;
	db1.WriteToCache(outstream);
	std::string outstring = get_string(outstream);
	std::istrstream instream(outstring.c_str());
	FamilyDatabase db3;
	db3.ReadFromCache(instream);
	std::ostrstream outstream2;
	db3.WriteToCache(outstream2);
	TEST(outstring == get_string(outstream2));
}


//=========================================================================
//	Test Driver
//=========================================================================

void test_Typography (void)
{
	test_Typography_Style();
	test_Typography_StyleInformation();
	test_Typography_LineSegment();
	test_Typography_GenericTextRenderingEngine();
	test_Typography_FamilyDatabase();
}
