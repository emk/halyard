#define WANT_FIVEL_TEST_INTERFACES 1

#include <iostream.h>
#include <string.h>

#include "ImlUnit.h"
#include "TEncoding.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  Test Data
//=========================================================================

static const int kEncodingCount = 3;
static const int kMaxEncodedCharLength = 10;

static char *encodings[kEncodingCount] = {
    "ISO-8859-1",
    "windows-1252",
    "macintosh"
};

typedef struct {
    char *input;
    char output[kEncodingCount][kMaxEncodedCharLength];
} EncodingTest;

// This macro wraps a single-byte string constant in all the grik required
// by each of the compilers we support.  The silly '(char)' is there to
// turn off some MSVC++ warnings.
#define BYTE_1(byte) {(char) (byte), (char) 0x00}

static EncodingTest encoding_tests[] = {

    // ASCII Entities
    {"&quot;",   {"\"", "\"", "\""}},
    {"&amp;",    {"&", "&", "&"}},
    {"&apos;",   {"'", "'", "'"}},
    {"&hyphen;", {"-", "-", "-"}},
    {"&period;", {".", ".", "."}},

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

    // (We'd like to support 'delta', but windows-1252 doesn't have it.)

    {NULL}
};


//=========================================================================
//  Error-reporting Callback Support
//=========================================================================

// We install this error reporting function when we don't expect errors.
static void DontWantErrors (const char *inBadString,
							size_t inBadPos,
							const char *inErrMsg)
{
    // We only run this code if an unexpected error occurs while
	// transforming text.
	cout << endl << "ENCODING ERROR: {{" << inErrMsg << "}} at "
		 << inBadPos << " in {{" << inBadString << "}}." << endl;
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
static void DoWantErrors (const char *inBadString,
						  size_t inBadPos,
						  const char *inErrMsg)
{
    TEST(inBadString != NULL);
    TEST(inBadPos >= 0 && inBadPos < strlen(inBadString));
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
    // Perform general tests that are the same for all encodings.
    for (int encoding_index = 0;
		 encoding_index < kEncodingCount;
		 encoding_index++)
    {
		TEncoding e1(encodings[encoding_index], DontWantErrors);
		TEST(e1.GetEncodingName() == encodings[encoding_index]);
		TEST(e1.TransformString("Hello!") == "Hello!");
		
		// Transform double hyphens into m-dash escape sequences.
		TEST(e1.FixSpecials("") == "");
		TEST(e1.FixSpecials("a-b") == "a-b");
		TEST(e1.FixSpecials("a--b") == "a&mdash;b");
		TEST(e1.FixSpecials("a---b") == "a&mdash;-b");
		TEST(e1.FixSpecials("a----b") == "a&mdash;&mdash;b");
		TEST(e1.FixSpecials("a -- b") == "a &mdash; b");
		TEST(e1.FixSpecials("--") == "&mdash;");
		
		// Transform '...' sequences into ellipses.
		TEST(e1.FixSpecials("...") == "&hellip;");
		TEST(e1.FixSpecials("....") == "&hellip;.");
		TEST(e1.FixSpecials(".....") == "&hellip;..");
		TEST(e1.FixSpecials("......") == "&hellip;&hellip;");
		TEST(e1.FixSpecials("Bye...") == "Bye&hellip;");
				
		// Education of quotes.
		// Double quotes within strings must be written as \042 (the octal
		// escape sequence) to avoid bugs in the expansion of the TEST macro
		// under Metrowerks C++ and Visual C++.
		TEST(e1.FixQuotes("") == "");
		TEST(e1.FixQuotes("\042Hello!\042") == "&ldquo;Hello!&rdquo;");
		TEST(e1.FixQuotes("Sam's") == "Sam&rsquo;s");
		TEST(e1.FixQuotes(" 'foo' ") == " &lsquo;foo&rsquo; ");
		TEST(e1.FixQuotes(" 'foo,' ") == " &lsquo;foo,&rsquo; ");
		TEST(e1.FixQuotes(" 'foo', ") == " &lsquo;foo&rsquo;, ");
		TEST(e1.FixQuotes(" \042foo\042 ") == " &ldquo;foo&rdquo; ");
		TEST(e1.FixQuotes(" \042foo,\042 ") == " &ldquo;foo,&rdquo; ");
		TEST(e1.FixQuotes(" \042foo\042, ") == " &ldquo;foo&rdquo;, ");
		TEST(e1.FixQuotes(" \042\'foo,\' he said!\042 ") ==
			 " &ldquo;&lsquo;foo,&rsquo; he said!&rdquo; ");
		TEST(e1.FixQuotes("\n\"Hi!\"") == "\n&ldquo;Hi!&rdquo;");
		TEST(e1.FixQuotes("\r\"Hi!\"") == "\r&ldquo;Hi!&rdquo;");
		
		// I know authors won't like these behaviors, but they match
		// most smart quoting algorithms, and they keep the code simple.
		TEST(e1.FixQuotes("class of '90") == "class of &lsquo;90");
		TEST(e1.FixQuotes("'till") == "&lsquo;till");
		
		// Test encoding of easy entities (the same on every platform).
		TEST(e1.EncodeEntities("") == "");
		TEST(e1.EncodeEntities("&quot;&amp;&apos;&hyphen;&period;") ==
			 "\"&'-.");
		TEST(e1.EncodeEntities("xyz&amp;abc") == "xyz&abc");
		
		// Test error-reporting for malformed and unknown entities.
		TEncoding e2(encodings[encoding_index], DoWantErrors);
		TEST(e2.EncodeEntities("&amp") == "&amp");
		TestErrorOccurred(0);
		TEST(e2.EncodeEntities("abc&amp;&amp ") == "abc&&amp ");
		TestErrorOccurred(8);
		TEST(e2.EncodeEntities("&nosuch;") == "&nosuch;");
		TestErrorOccurred(0);
		TEST(e2.EncodeEntities("&amp;&nosuch;") == "&&nosuch;");
		TestErrorOccurred(5);
		TEST(e2.EncodeEntities("abc&nosuch;&amp; ") == "abc&nosuch;& ");
		TestErrorOccurred(3);
		
		// (Two errors occur here, but our test case only notices the
		// second of the two.)
		TEST(e2.EncodeEntities("&nosuch;&nosuch;") == "&nosuch;&nosuch;");
		TestErrorOccurred(8); 
		
		// Do the brute-force test of all supported entities.
		for (EncodingTest *test = encoding_tests; test->input != NULL; test++)
			TEST(e1.EncodeEntities(test->input) ==
				 test->output[encoding_index]);
    }
}
