// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#include <ctype.h>

#include "TTemplateUtils.h"

using namespace Halyard;

REGISTER_TEST_CASE_FILE(TTemplateUtils);

bool Halyard::StringIComp(const std::string &inStr1,
                          const std::string &inStr2)
{
    return MakeStringLowercase(inStr1) == MakeStringLowercase(inStr2);
}

void Halyard::StringLTrim(std::string &ioStr) {
    while (ioStr.size() && (ioStr[0] == ' ' || ioStr[0] == '\t'))
        ioStr = ioStr.substr(1);
}

bool Halyard::StringStartsWith(const std::string &inStr1, 
                               const std::string &inStr2) 
{
    return (inStr1.find_first_of(inStr2) == 0);
}

std::string Halyard::MakeStringLowercase(std::string inString)
{
    std::transform(inString.begin(), inString.end(), inString.begin(),
				   tolower);
	return inString;
}

std::string Halyard::MakeQuotedString(const std::string& inString)
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
