#define WANT_FIVEL_TEST_INTERFACES 1

#include <iostream.h>

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

static EncodingTest encoding_tests[] = {

    // ASCII Entities
    {"&quot;",   {"\"", "\"", "\""}},
    {"&amp;",    {"&", "&", "&"}},
    {"&apos;",   {"'", "'", "'"}},
    {"&hyphen;", {"-", "-", "-"}},
    {"&period;", {".", ".", "."}},

    // Internally-Referenced Entities
    {"&mdash;",  {"--",         {0x97, 0x00}, {0xD1, 0x00}}},
    {"&lsquo;",  {"`",          {0x91, 0x00}, {0xD4, 0x00}}},
    {"&rsquo;",  {"'",          {0x92, 0x00}, {0xD5, 0x00}}},
    {"&ldquo;",  {"\"",         {0x93, 0x00}, {0xD2, 0x00}}},
    {"&rdquo;",  {"\"",         {0x94, 0x00}, {0xD3, 0x00}}},
    {"&hellip;", {"...",        {0x85, 0x00}, {0xC9, 0x00}}}, // TODO - "..."

    // Common Script Entities
    {"&copy;",   {{0xA9, 0x00}, {0xA9, 0x00}, {0xA9, 0x00}}},
    {"&reg;",    {{0xAE, 0x00}, {0xAE, 0x00}, {0xA8, 0x00}}},
    {"&trade;",  {"<TM>",       {0x99, 0x00}, {0xAA, 0x00}}},
    {"&bull;",   {"*",          {0x95, 0x00}, {0xA5, 0x00}}},
    {"&sect;",   {{0xA7, 0x00}, {0xA7, 0x00}, {0xA4, 0x00}}},
    {"&ndash;",  {"-",          {0x96, 0x00}, {0xD0, 0x00}}},
    {"&dagger;", {"<t>",        {0x86, 0x00}, {0xA0, 0x00}}},
    {"&micro;",  {{0xB5, 0x00}, {0xB5, 0x00}, {0xB5, 0x00}}},
    {"&para;",   {{0xB6, 0x00}, {0xB6, 0x00}, {0xA6, 0x00}}},
    {"&eacute;", {{0xE9, 0x00}, {0xE9, 0x00}, {0x8E, 0x00}}},
    {"&Ccedil;", {{0xC7, 0x00}, {0xC7, 0x00}, {0x82, 0x00}}},
    {"&ccedil;", {{0xE7, 0x00}, {0xE7, 0x00}, {0x8D, 0x00}}},

    // (We'd like to support 'delta', but windows-1252 doesn't have it.)

    {NULL}
};


//=========================================================================
//  Error-reporting Callback Support
//=========================================================================

// We install this error reporting function when we don't expect errors.
static void DontWantErrors (const char *inBadString,
							int inBadPos,
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
						  int inBadPos,
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
		TEST(e1.FixQuotes("") == "");
		TEST(e1.FixQuotes("\"Hello!\"") == "&ldquo;Hello!&rdquo;");
		TEST(e1.FixQuotes("Sam's") == "Sam&rsquo;s");
		TEST(e1.FixQuotes(" 'foo' ") == " &lsquo;foo&rsquo; ");
		TEST(e1.FixQuotes(" 'foo,' ") == " &lsquo;foo,&rsquo; ");
		TEST(e1.FixQuotes(" 'foo', ") == " &lsquo;foo&rsquo;, ");
		TEST(e1.FixQuotes(" \"foo\" ") == " &ldquo;foo&rdquo; ");
		TEST(e1.FixQuotes(" \"foo,\" ") == " &ldquo;foo,&rdquo; ");
		TEST(e1.FixQuotes(" \"foo\", ") == " &ldquo;foo&rdquo;, ");
		TEST(e1.FixQuotes(" \"\'foo,\' he said!\" ") ==
			 " &ldquo;&lsquo;foo,&rsquo; he said!&rdquo; ");
		
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
