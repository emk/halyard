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

typedef std::pair<StyleInformation*,StyledTextSpan> InfoAndSpan;

static InfoAndSpan make_span(const std::wstring &inString)
{
	int len = inString.length();
	const wchar_t *str = inString.c_str();
	StyleInformation *styleInfo =
		new StyleInformation(Style("Nimbus Roman No9 L", 12));
	styleInfo->EndStyleAt(len);
	return InfoAndSpan(styleInfo,
					   StyledTextSpan(StyledCharIterator(str,
														 styleInfo->begin()),
									  StyledCharIterator(str + len,
														 styleInfo->end())));
}

static LineSegment make_test_seg(const StyledTextSpan &inBaseSpan,
								 int inBegin, int inEnd,
								 bool inIsLineBreak = false,
								 bool inDiscardAtEndOfLine = false,
								 bool inNeedsHyphenAtEndOfLine = false)
{
	StyledCharIterator begin = inBaseSpan.begin;
	for (int i = 0; i < inBegin; i++)
		++begin;
	StyledCharIterator end = inBaseSpan.begin;
	for (int j = 0; j < inEnd; j++)
		++end;
	return LineSegment(StyledTextSpan(begin, end), inIsLineBreak,
					   inDiscardAtEndOfLine, inNeedsHyphenAtEndOfLine);
}

#define TEST_SEGMENT(seg,span,from,to,b1,b2,b3) \
    TEST(seg == make_test_seg(span, from, to, b1, b2, b3))

static void test_Typography_LineSegment (void) 
{
	// Simple words & whitespace.
	std::wstring text1 = L"abc def  ghi";
	InfoAndSpan infospan1 = make_span(text1);
	StyledTextSpan span1 = infospan1.second;
	LineSegmentIterator iter1(span1);
	LineSegment seg1;
	TEST(iter1.NextElement(&seg1));
	TEST_SEGMENT(seg1, span1, 0, 3, false, false, false);
	TEST(iter1.NextElement(&seg1));
	TEST_SEGMENT(seg1, span1, 3, 4, false, true,  false);
	TEST(iter1.NextElement(&seg1));
	TEST_SEGMENT(seg1, span1, 4, 7, false, false, false);
	TEST(iter1.NextElement(&seg1));
	TEST_SEGMENT(seg1, span1, 7, 9, false, true,  false);
	TEST(iter1.NextElement(&seg1));
	TEST_SEGMENT(seg1, span1, 9, 12, false, false,  false);
	TEST(!iter1.NextElement(&seg1));
	delete infospan1.first;

	// Basic hyphenation.
	std::wstring text2 = L"abc-def- ghi---";
	InfoAndSpan infospan2 = make_span(text2);
	StyledTextSpan span2 = infospan2.second;
	LineSegmentIterator iter2(span2);
	LineSegment seg2;
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, span2, 0, 4, false, false, false);
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, span2, 4, 8, false, false, false);
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, span2, 8, 9, false, true, false);
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, span2, 9, 13, false, false, false);
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, span2, 13, 14, false, false, false);
	TEST(iter2.NextElement(&seg2));
	TEST_SEGMENT(seg2, span2, 14, 15, false, false, false);
	TEST(!iter2.NextElement(&seg2));
	delete infospan2.first;

	// Explicit newlines.
	std::wstring text3 = L" abc\ndef \n\n ghi";
	InfoAndSpan infospan3 = make_span(text3);
	StyledTextSpan span3 = infospan3.second;
	LineSegmentIterator iter3(span3);
	LineSegment seg3;
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, span3, 0, 1, false, true, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, span3, 1, 4, false, false, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, span3, 4, 5, true, false, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, span3, 5, 8, false, false, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, span3, 8, 9, false, true, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, span3, 9, 10, true, false, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, span3, 10, 11, true, false, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, span3, 11, 12, false, true, false);
	TEST(iter3.NextElement(&seg3));
	TEST_SEGMENT(seg3, span3, 12, 15, false, false, false);
	TEST(!iter3.NextElement(&seg3));
	delete infospan3.first;

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
	InfoAndSpan infospan4 = make_span(text4);
	StyledTextSpan span4 = infospan4.second;
	LineSegmentIterator iter4(span4);
	LineSegment seg4;
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, span4, 0, 3, false, false, true);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, span4, 4, 7, false, false, true);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, span4, 8, 9, false, true, false);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, span4, 9, 12, false, false, false);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, span4, 12, 13, true, false, false);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, span4, 14, 17, false, false, true);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, span4, 20, 21, true, false, false);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, span4, 21, 22, false, true, false);
	TEST(iter4.NextElement(&seg4));
	TEST_SEGMENT(seg4, span4, 23, 24, false, true, false);
	TEST(!iter4.NextElement(&seg4));
	delete infospan4.first;

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
	TestTextRenderingEngine(const StyledTextSpan &inSpan,
							Distance inLineLength,
							Justification inJustification)
		: GenericTextRenderingEngine(inSpan, inLineLength,
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

	Distance result = 0;
	for (StyledCharIterator iter = inSegment->span.begin;
		 iter != inSegment->span.end; ++iter)
		++result;
	
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

	StyledCharIterator new_end = ioRemaining->span.begin;
	for (int i = 0; i < GetLineLength() - 1; ++i)
		++new_end;
	outExtracted->SetLineSegment(StyledTextSpan(ioRemaining->span.begin,
												new_end),
								 false, false, true);
	TEST(outExtracted->span.end != ioRemaining->span.end);
	ioRemaining->span.begin = outExtracted->span.end;	
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
		for (StyledCharIterator iter = seg.span.begin;
			 iter != seg.span.end; ++iter)
			mRenderedText += (*iter).value;
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

	// Build a dummy StyleInformation object.
	Style style("Nimbus Roman No9 L", 12);
	StyleInformation styleInfo(style);
	styleInfo.EndStyleAt(s.length());

	// Set up the rest of our parameters and call the engine.
	StyledCharIterator s_begin(s.c_str(), styleInfo.begin());
	StyledCharIterator s_end(s.c_str() + s.length(), styleInfo.end());
	TestTextRenderingEngine e(StyledTextSpan(s_begin, s_end),
							  width, justification);
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
