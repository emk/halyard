// -*- Mode: C++; tab-width: 4; -*-

#include "CommonHeaders.h"

#include <string.h>

#include "TTemplateUtils.h"
#include "ImlUnit.h"

USING_NAMESPACE_FIVEL

extern void test_TTemplateUtils (void);

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


void test_TTemplateUtils (void)
{
    TEST(MakeStringLowercase("aBCd") == std::string("abcd"));

	for (QuotedStringTestData *test = &quoted_string_tests[0];
		 test->input != NULL; test++)
	{
		TEST(MakeQuotedString(test->input) == std::string(test->output));
	}

	int i = 0;
	{
		StValueRestorer<int> restore_i(i);
		i = 1;
		TEST(i == 1);
	}
	TEST(i == 0);
}
