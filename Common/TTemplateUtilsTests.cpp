// -*- Mode: C++; tab-width: 4; -*-

#include "CommonHeaders.h"

#include "TTemplateUtils.h"
#include "TestCase.h"

USING_NAMESPACE_FIVEL

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
