#include "ImlUnit.h"
#include "TEncoding.h"

USING_NAMESPACE_FIVEL

//=========================================================================
//  Test Data
//=========================================================================

static const int kEncodingCount = 3;

static char *encodings[kEncodingCount] = {
    "ISO-8859-1",
    "windows-1252",
    "macintosh"
};

typedef struct {
    char *input;
    char *output[kEncodingCount];
} EncodingTest;

static EncodingTest encoding_tests[] = {

    // ASCII Entities
    {"&quot;", {"\"", "\"", "\""}},
    {"&amp;", {"&", "&", "&"}},
    {"&#39;", {"'", "'", "'"}},
    {"&#45;", {"-", "-", "-"}},

    // Internally-Referenced Entities
    // mdash
    // lsquo
    // rsquo
    // ldquo
    // rdquo

    // Common Script Entities
    // ellipsis
    // copyright
    // registered
    // trademark
    // bullet
    // section
    // ndash
    
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
    // We should never run this code.
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
	TEST(e1.EncodeString("Hello!") == "Hello!");

	// Transform double hyphens into m-dash escape sequences.
	TEST(e1.FixMDashes("a-b") == "a-b");
	TEST(e1.FixMDashes("a--b") == "a&mdash;b");
	TEST(e1.FixMDashes("a---b") == "a&mdash;-b");
	TEST(e1.FixMDashes("a----b") == "a&mdash;&mdash;b");
	TEST(e1.FixMDashes("a -- b") == "a &mdash; b");
	TEST(e1.FixMDashes("--") == "&mdash;");

	// Education of quotes.
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
	TEST(e1.EncodeEntities("&quot;&amp;&#39;&#45;") == "\"&'-");
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
