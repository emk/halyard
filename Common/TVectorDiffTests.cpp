// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
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

#include "ImlUnit.h"
#include "TVectorDiff.h"

using namespace Halyard;

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


