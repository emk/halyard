// -*- Mode: C++; tab-width: 4; -*-

#include "CommonHeaders.h"

#include <ctype.h>

#include "TTemplateUtils.h"

USING_NAMESPACE_FIVEL

REGISTER_TEST_CASE_FILE(TTemplateUtils);

bool StringIComp(const std::string &inStr1, const std::string &inStr2) {
    return MakeStringLowercase(inStr1) == MakeStringLowercase(inStr2);
}

void StringLTrim(std::string &ioStr) {
    while (ioStr.size() && (ioStr[0] == ' ' || ioStr[0] == '\t'))
        ioStr = ioStr.substr(1);
}

bool StringStartsWith(const std::string &inStr1, 
					  const std::string &inStr2) 
{
    return (inStr1.find_first_of(inStr2) == 0);
}

std::string FIVEL_NS MakeStringLowercase(std::string inString)
{
    std::transform(inString.begin(), inString.end(), inString.begin(),
				   tolower);
	return inString;
}

std::string FIVEL_NS MakeQuotedString(const std::string& inString)
{
	std::string result = "\"";
	for (std::string::size_type pos = 0; pos < inString.length(); pos++)
	{
		char c = inString[pos];
		if (c == '\"')
			result += "\\\"";
		else if (c == '\\')
			result += "\\\\";
		else
			result += c;
	}
	result += "\"";
	return result;
}


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

struct QuotedStringTestData
{
	char *input;
	char *output;
};

QuotedStringTestData quoted_string_tests[] = {
	{"", "\"\""},
	{"\\", "\"\\\\\""},
	{"foo\"bar\"", "\"foo\\\"bar\\\"\""},
	{"'", "\"'\""},
	{NULL, NULL}
};


BEGIN_TEST_CASE(TestTTemplateUtils, TestCase) {
    CHECK_EQ(MakeStringLowercase("aBCd"), std::string("abcd"));

	for (QuotedStringTestData *test = &quoted_string_tests[0];
		 test->input != NULL; test++)
		CHECK_EQ(MakeQuotedString(test->input), std::string(test->output));

	int i = 0;
	{
		StValueRestorer<int> restore_i(i);
		i = 1;
		CHECK_EQ(i, 1);
	}
	CHECK_EQ(i, 0);
} END_TEST_CASE(TestTTemplateUtils);

#endif // BUILD_TEST_CASES
