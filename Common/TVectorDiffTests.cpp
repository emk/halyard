// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "ImlUnit.h"
#include "TCommon.h"
#include "TVectorDiff.h"

USING_NAMESPACE_FIVEL

extern void test_TVectorDiff (void);

static void fill_vec(const char *inData, std::vector<char> &outVec)
{
    for (int i = 0; inData[i] != 0; i++)
		outVec.push_back(inData[i]);
}

static void TEST_LCS(const char *v1, const char *v2, const char *lcs)
{
    std::vector<char> v_v1, v_v2, v_lcs, result;
    fill_vec(v1, v_v1);
    fill_vec(v2, v_v2);
    fill_vec(lcs, v_lcs);

    LargestCommonSubsequence<char>(v_v1, v_v2, result);
    TEST(v_lcs == result);
}

void test_TVectorDiff (void)
{
	TEST_LCS("", "", "");
	TEST_LCS("a", "", "");
	TEST_LCS("", "a", "");
	TEST_LCS("a", "a", "a");
	TEST_LCS("ab", "ac", "a");
	TEST_LCS("abcdef", "abfcde", "abcde");
	TEST_LCS("abXcdXef", "YYabcdeYYYf", "abcdef");
}


