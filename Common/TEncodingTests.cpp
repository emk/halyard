// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

#include "ImlUnit.h"
#include "TEncoding.h"
#include "TUtilities.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  Test Data
//=========================================================================

typedef struct {
    wchar_t *input;
    wchar_t *output;
} EncodingTest;

static EncodingTest encoding_tests[] = {

    // ASCII Entities
    {L"&quot;",   L"\""},
    {L"&amp;",    L"&"},
    {L"&apos;",   L"\'"},
    {L"&hyphen;", L"-"},
    {L"&period;", L"."},

    // Old test cases from back when we supported three non-Unicode
    // encodings.

    /*
    // Internally-Referenced Entities
    {"&mdash;",  {"--",         BYTE_1(0x97), BYTE_1(0xD1)}},
    {"&lsquo;",  {"`",          BYTE_1(0x91), BYTE_1(0xD4)}},
    {"&rsquo;",  {"'",          BYTE_1(0x92), BYTE_1(0xD5)}},
    {"&ldquo;",  {"\"",         BYTE_1(0x93), BYTE_1(0xD2)}},
    {"&rdquo;",  {"\"",         BYTE_1(0x94), BYTE_1(0xD3)}},
    {"&hellip;", {"...",        BYTE_1(0x85), BYTE_1(0xC9)}},

    // Common Script Entities
    {"&copy;",   {BYTE_1(0xA9), BYTE_1(0xA9), BYTE_1(0xA9)}},
    {"&reg;",    {BYTE_1(0xAE), BYTE_1(0xAE), BYTE_1(0xA8)}},
    {"&trade;",  {"<TM>",       BYTE_1(0x99), BYTE_1(0xAA)}},
    {"&bull;",   {"*",          BYTE_1(0x95), BYTE_1(0xA5)}},
    {"&sect;",   {BYTE_1(0xA7), BYTE_1(0xA7), BYTE_1(0xA4)}},
    {"&ndash;",  {"-",          BYTE_1(0x96), BYTE_1(0xD0)}},
    {"&dagger;", {"<t>",        BYTE_1(0x86), BYTE_1(0xA0)}},
    {"&micro;",  {BYTE_1(0xB5), BYTE_1(0xB5), BYTE_1(0xB5)}},
    {"&para;",   {BYTE_1(0xB6), BYTE_1(0xB6), BYTE_1(0xA6)}},
    {"&eacute;", {BYTE_1(0xE9), BYTE_1(0xE9), BYTE_1(0x8E)}},
    {"&Ccedil;", {BYTE_1(0xC7), BYTE_1(0xC7), BYTE_1(0x82)}},
    {"&ccedil;", {BYTE_1(0xE7), BYTE_1(0xE7), BYTE_1(0x8D)}},
    */

    {NULL}
};


//=========================================================================
//  Error-reporting Callback Support
//=========================================================================

// We install this error reporting function when we don't expect errors.
static void DontWantErrors (const std::wstring &inBadString,
							size_t inBadPos,
							const char *inErrMsg)
{
	std::string bad_str(ConstructString<char>(inBadString.begin(),
		                                      inBadString.end()));

    // We only run this code if an unexpected error occurs while
	// transforming text.
	std::cout << std::endl << "ENCODING ERROR: {{" << inErrMsg << "}} at "
			  << inBadPos << " in {{" << bad_str << "}}." << std::endl;
    TEST(false);
}

// True if an error occured since the last call to TestErrorOccurred.
static bool gErrorOccurred = false;

// The position at which the last error occurred.
static int gErrorPosition = 0;

// Test whether an error occurred (and whether it occured in the right
// position), and reset our internal error flag.
static void TestErrorOccurred (int inAtPos)
{
    TEST(gErrorOccurred);
    TEST(inAtPos == gErrorPosition);
    gErrorOccurred = false;
}

// We install this error reporting function when we don't expect errors.
static void DoWantErrors (const std::wstring &inBadString,
						  size_t inBadPos,
						  const char *inErrMsg)
{
    TEST(inBadPos >= 0 && inBadPos < inBadString.length());
    TEST(inErrMsg != NULL);
    gErrorOccurred = true;
    gErrorPosition = inBadPos;
}


//=========================================================================
//  Test Cases
//=========================================================================

extern void test_TEncoding (void);

void test_TEncoding (void) 
{
    TEncoding e1(DontWantErrors);
    TEST(e1.TransformString(L"Hello!") == L"Hello!");
    
    // Transform double hyphens into m-dash escape sequences.
    TEST(e1.FixSpecials(L"") == L"");
    TEST(e1.FixSpecials(L"a-b") == L"a-b");
    TEST(e1.FixSpecials(L"a--b") == L"a&mdash;b");
    TEST(e1.FixSpecials(L"a---b") == L"a&mdash;-b");
    TEST(e1.FixSpecials(L"a----b") == L"a&mdash;&mdash;b");
    TEST(e1.FixSpecials(L"a -- b") == L"a &mdash; b");
    TEST(e1.FixSpecials(L"--") == L"&mdash;");
    
    // Transform '...' sequences into ellipses.
    TEST(e1.FixSpecials(L"...") == L"&hellip;");
    TEST(e1.FixSpecials(L"....") == L"&hellip;.");
    TEST(e1.FixSpecials(L".....") == L"&hellip;..");
    TEST(e1.FixSpecials(L"......") == L"&hellip;&hellip;");
    TEST(e1.FixSpecials(L"Bye...") == L"Bye&hellip;");
    
    // Education of quotes.
    // Double quotes within strings must be written as \042 (the octal
    // escape sequence) to avoid bugs in the expansion of the TEST macro
    // under Metrowerks C++ and Visual C++.
    TEST(e1.FixQuotes(L"") == L"");
    TEST(e1.FixQuotes(L"\042Hello!\042") == L"&ldquo;Hello!&rdquo;");
    TEST(e1.FixQuotes(L"Sam's") == L"Sam&rsquo;s");
    TEST(e1.FixQuotes(L" 'foo' ") == L" &lsquo;foo&rsquo; ");
    TEST(e1.FixQuotes(L" 'foo,' ") == L" &lsquo;foo,&rsquo; ");
    TEST(e1.FixQuotes(L" 'foo', ") == L" &lsquo;foo&rsquo;, ");
    TEST(e1.FixQuotes(L" \042foo\042 ") == L" &ldquo;foo&rdquo; ");
    TEST(e1.FixQuotes(L" \042foo,\042 ") == L" &ldquo;foo,&rdquo; ");
    TEST(e1.FixQuotes(L" \042foo\042, ") == L" &ldquo;foo&rdquo;, ");
    TEST(e1.FixQuotes(L" \042\'foo,\' he said!\042 ") ==
         L" &ldquo;&lsquo;foo,&rsquo; he said!&rdquo; ");
    TEST(e1.FixQuotes(L"\n\042Hi!\042") == L"\n&ldquo;Hi!&rdquo;");
    TEST(e1.FixQuotes(L"\r\042Hi!\042") == L"\r&ldquo;Hi!&rdquo;");
    
    // I know authors won't like these behaviors, but they match
    // most smart quoting algorithms, and they keep the code simple.
    TEST(e1.FixQuotes(L"class of '90") == L"class of &lsquo;90");
    TEST(e1.FixQuotes(L"'till") == L"&lsquo;till");
    
    // Test encoding of easy entities (the same on every platform).
    TEST(e1.EncodeEntities(L"") == L"");
    TEST(e1.EncodeEntities(L"&quot;&amp;&apos;&hyphen;&period;") ==
         L"\"&'-.");
    TEST(e1.EncodeEntities(L"xyz&amp;abc") == L"xyz&abc");
    
    // Test error-reporting for malformed and unknown entities.
    TEncoding e2(DoWantErrors);
    TEST(e2.EncodeEntities(L"&amp") == L"&amp");
    TestErrorOccurred(0);
    TEST(e2.EncodeEntities(L"abc&amp;&amp ") == L"abc&&amp ");
    TestErrorOccurred(8);
    TEST(e2.EncodeEntities(L"&nosuch;") == L"&nosuch;");
    TestErrorOccurred(0);
    TEST(e2.EncodeEntities(L"&amp;&nosuch;") == L"&&nosuch;");
    TestErrorOccurred(5);
    TEST(e2.EncodeEntities(L"abc&nosuch;&amp; ") == L"abc&nosuch;& ");
    TestErrorOccurred(3);
    
    // (Two errors occur here, but our test case only notices the
    // second of the two.)
    // TODO - What's going on here?
    TEST(e2.EncodeEntities(L"&nosuch;&nosuch;") == L"&nosuch;&nosuch;");
    TestErrorOccurred(8); 
    
    // Do the brute-force test of all supported entities.
    for (EncodingTest *test = encoding_tests; test->input != NULL; test++)
        TEST(e1.EncodeEntities(test->input) == test->output);
}
