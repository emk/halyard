// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

/// @file TTextConv_H
///
/// The code in this file should be correct, as far as it goes, but it
/// ignores lots of exciting issues we'll need to deal with someday:
///
///   * Do we even want a type named utf16_string?  Is std::wstring better?
///   * What do we want to do with non-BMP characters (i.e., > 0xFFFF)?
///   * Do we want explicit utf32 versions of these functions?  And if so,
///     what character type should they use?

//=========================================================================
//  Additional License Terms
//=========================================================================
//  The UTF-8 code in this file is adapted from the xmlrpc-c library
//  version 0.9.10, which I wrote before I worked for Dartmouth.  I'm
//  reusing this code because (1) it's open source, and (2) it has much
//  better test cases than most of the UTF-8 conversion routines floating
//  around on the net.
//
//  This copyright notice must be preserved, in addition to the usual
//  Dartmouth Trustees copyright.  (Actually, it's highly likely that the
//  First Peer copyright doesn't apply to this code, but you'd have to dig
//  through the CVS logs of xmlrpc-c to be certain one way or another.)
//
//  Copyright (C) 2001 by Eric Kidd. All rights reserved.
//  Copyright (C) 2001 by First Peer, Inc. All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//  1. Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. The name of the author may not be used to endorse or promote products
//     derived from this software without specific prior written permission. 
//  
//  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
//  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS
//  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
//  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
//  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
//  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


#include "CommonHeaders.h"

#include <stdlib.h>

using namespace Halyard;

REGISTER_TEST_CASE_FILE(TTextConv);


//=========================================================================
//  UTF-8 Conversion Tables
//=========================================================================
//  We use a variety of tables and constants to help decode and validate
//  UTF-8 data.
//
//  We could use the UTF-8 converter in the latest Boost, but it doesn't
//  catch overlong encodings.  Since that's a basic UTF-8 security issue,
//  any halfway competent UTF-8 library should get it right.  Since Boost
//  library doesn't get it right, we can conclude that Boost isn't ready
//  for UTF-8 prime time.

// The number of bytes in a UTF-8 sequence starting with the character used
// as the array index.  A zero entry indicates an illegal initial byte.
// This table was generated using a Perl script and information from the
// UTF-8 standard.
//
// Fredrik Lundh's UTF-8 decoder Python 2.0 uses a similar table.  But
// since Python 2.0 has the icky CNRI license, I regenerated this
// table from scratch and wrote my own decoder.
static unsigned char utf8_seq_length[256] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 0, 0
};

// The minimum legal character value for a UTF-8 sequence of the given
// length.  We have to check this to avoid accepting "overlong" UTF-8
// sequences, which use more bytes than necessary to encode a given
// character.  Such sequences are commonly used by evil people to bypass
// filters and security checks.  This table is based on the UTF-8-test.txt
// file by Markus Kuhn <mkuhn@acm.org>.
static wchar_t utf8_min_char_for_length[4] = {
    0,          // Length 0: Not used (meaningless)
    0x0000,     // Length 1: Not used (special-cased)
    0x0080,     // Length 2
    0x0800      // Length 3

#if 0
    // These are only useful on systems where wchar_t is 32-bits wide
    // and supports full UCS-4.
    0x00010000, // Length 4
    0x00200000, // Length 5
    0x04000000  // Length 6
#endif
};

// This is the maximum legal 16-byte (UCS-2) character.  Again, this
// information is based on UTF-8-test.txt.
#define UCS2_MAX_LEGAL_CHARACTER (0xFFFD)

// First and last UTF-16 surrogate characters.  These are *not* legal UCS-2
// characters--they're used to code for UCS-4 characters when using UTF-16.
// They should never appear in decoded UTF-8 data!  Again, these could
// hypothetically be used to bypass security measures on some machines.
// Based on UTF-8-test.txt.
#define UTF16_FIRST_SURROGATE (0xD800)
#define UTF16_LAST_SURROGATE  (0xDFFF)

// Is the character 'c' a UTF-8 continuation character?
#define IS_CONTINUATION(c) (((c) & 0xC0) == 0x80)

// Maximum number of bytes needed to encode a supported character.
#define MAX_ENCODED_BYTES (3)


//=========================================================================
//  String Conversion Code
//=========================================================================
//  We provide our own string conversion functions, because the best way to
//  implement them varies wildly, and many UTF-8 converters are buggy.

/// Convert UTF-8 to UTF-16.
utf16_string Halyard::utf16_from_utf8(const utf8_string &utf8) {
    utf16_string result;
    result.reserve(utf8.size());

    // Suppress GCC warning about possibly undefined variable.
    wchar_t wc = 0;

    size_t i = 0;
    size_t out_pos = 0;
    while (i < utf8.size()) {
        char init = utf8[i];
        if ((init & 0x80) == 0x00) {
            // Convert ASCII character to wide character.
            wc = init;
            i++;
        } else {
            
            // Look up the length of this UTF-8 sequence.
            size_t length = utf8_seq_length[(unsigned char) init];
            
            // Check to make sure we have enough bytes to convert.
            CHECK(i + length <= utf8.size(), "Truncated UTF-8 sequence");
            
            // Decode a multibyte UTF-8 sequence.
            char con1;
            char con2;
            switch (length) {
                case 0:
                    THROW("Invalid UTF-8 initial byte");
                    
                case 2:
                    // 110xxxxx 10xxxxxx
                    con1 = utf8[i+1];
                    CHECK(IS_CONTINUATION(con1), "UTF-8 sequence too short");
                    wc = ((((wchar_t) (init & 0x1F)) <<  6) |
                          (((wchar_t) (con1 & 0x3F))));
                    break;
                    
                case 3:
                    // 1110xxxx 10xxxxxx 10xxxxxx
                    con1 = utf8[i+1];
                    con2 = utf8[i+2];
                    CHECK(IS_CONTINUATION(con1) && IS_CONTINUATION(con2),
                          "UTF-8 sequence too short");
                    wc = ((((wchar_t) (init & 0x0F)) << 12) |
                          (((wchar_t) (con1 & 0x3F)) <<  6) |
                          (((wchar_t) (con2 & 0x3F))));
                    break;
                    
                case 4:
                    // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                case 5:
                    // 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
                case 6:
                    // 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
                    THROW("UCS-4 characters not supported");
                    
                default:
                    gLog.FatalError("Error in UTF-8 decoder tables");
            }
		    
            // Advance to the end of the sequence.
            i += length;
            
            // Check for illegal UCS-2 characters.
            CHECK(wc <= UCS2_MAX_LEGAL_CHARACTER,
                  "UCS-2 characters > U+FFFD are illegal");
            
            // Check for UTF-16 surrogates.
            CHECK(wc < UTF16_FIRST_SURROGATE || UTF16_LAST_SURROGATE < wc,
                  "UTF-16 surrogates may not appear in UTF-8 data");
            
            // Check for overlong sequences.
            CHECK(wc >= utf8_min_char_for_length[length],
                  "Overlong UTF-8 sequence not allowed");
        }
        
        // Append the decoded character to our result.
        result.push_back(wc);
    }
    return result;
}

/// Convert UTF-16 to UTF-8.
utf8_string Halyard::utf8_from_utf16(const utf16_string &utf16) {
    utf8_string result;
    result.reserve(utf16.size());

    // Output our characters.
    for (size_t i = 0; i < utf16.size(); i++) {
        wchar_t wc = utf16[i];
        if (wc <= 0x007F) {
            result.push_back(wc & 0x7F);
        } else if (wc <= 0x07FF) {
            // 110xxxxx 10xxxxxx
            result.push_back(0xC0 | (wc >> 6));
            result.push_back(0x80 | (wc & 0x3F));
        } else if (wc <= 0xFFFF) {
            // 1110xxxx 10xxxxxx 10xxxxxx
            result.push_back(0xE0 | (wc >> 12));
            result.push_back(0x80 | ((wc >> 6) & 0x3F));
            result.push_back(0x80 | (wc & 0x3F));
        } else {
            THROW("Don't know how to encode UCS-4 characters yet");
        }
    }

    return result;
}

/// Convert UTF-16 to native multibyte encoding.
/// @see utf16_from_multibyte
std::string Halyard::multibyte_from_utf16(const utf16_string &utf16) {
    // Figure out how much space we'll need.  Yes, we really are comparing
    // a size_t value against -1--that's what the manual says.
    size_t dest_size = ::wcstombs(NULL, utf16.c_str(), utf16.size());
    CHECK(dest_size != -1, "Malformed Unicode string");

    // Perform the conversion.
    std::string dest(" ", dest_size);
    size_t dest_size2 = ::wcstombs(&dest[0], utf16.c_str(), utf16.size());
    ASSERT(dest_size == dest_size2);
    return dest;
}

/// Convert native multibyte encoding to UTF-16.  Note that on some
/// platforms, this may actually give us UTF-32, or even something
/// completely undefined.  We'll deal with those problems when we encounter
/// them--there's no point in writing a whole bunch of code that won't
/// actually be used right away.
utf16_string Halyard::utf16_from_multibyte(const std::string &str) {
    // Figure out how much space we'll need.  Yes, we really are comparing
    // a size_t value against -1--that's what the manual says.
    size_t dest_size = ::mbstowcs(NULL, str.c_str(), str.size());
    CHECK(dest_size != -1, "Malformed multibyte string");

    // Perform the conversion.
    utf16_string dest(L" ", dest_size);
    size_t dest_size2 = ::mbstowcs(&dest[0], str.c_str(), str.size());
    ASSERT(dest_size == dest_size2);
    return dest;
}

/// Convert UTF-8 to native multibyte encoding.
std::string Halyard::multibyte_from_utf8(const utf8_string &utf8) {
    return multibyte_from_utf16(utf16_from_utf8(utf8));
}

/// Convert native multibyte encoding to UTF-8.
utf8_string Halyard::utf8_from_multibyte(const std::string &str) {
    return utf8_from_utf16(utf16_from_multibyte(str));
}


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

BEGIN_TEST_CASE(TestMultibyteConversion, TestCase) {
    // Check simple conversions to UTF-16.
	CHECK_EQ(utf16_from_multibyte("") == L"", true);
	CHECK_EQ(utf16_from_multibyte("abc") == L"abc", true);

    // Check simple conversions to native multibyte.
	CHECK_EQ(multibyte_from_utf16(L""), "");
	CHECK_EQ(multibyte_from_utf16(L"abc"), "abc");

    // Since we don't actually *know* what the native multibyte encoding
    // is, we're somewhat limited in the kinds of tests we can write.
} END_TEST_CASE(TestMultibyteConversion);

// We need to test our UTF-8 decoder thoroughly.  Most of these test cases
// are taken from the UTF-8-test.txt file by Markus Kuhn <mkuhn@acm.org>:
//
//      http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt

typedef struct {
    char *utf8;
    wchar_t wcs[16];
} utf8_and_wcs;

static const utf8_and_wcs good_utf8[] = {

    // Greek 'kosme'.
    {"\316\272\341\275\271\317\203\316\274\316\265",
     {0x03BA, 0x1F79, 0x03C3, 0x03BC, 0x03B5, 0}},

    // First sequences of a given length.
    // '\000' is not a legal C string.
    {"\302\200", {0x0080, 0}},
    {"\340\240\200", {0x0800, 0}},

    // Last sequences of a given length.
    {"\177", {0x007F, 0}},
    {"\337\277", {0x07FF, 0}},
    // 0xFFFF is not a legal Unicode character.

    // Other boundry conditions.
    {"\001", {0x0001, 0}},
    {"\355\237\277", {0xD7FF, 0}},
    {"\356\200\200", {0xE000, 0}},
    {"\357\277\275", {0xFFFD, 0}},

    // Other random test cases.
    {"", {0}},
    {"abc", {0x0061, 0x0062, 0x0063, 0}},
    {"[\302\251]", {0x005B, 0x00A9, 0x005D, 0}},
    
    {NULL, {0}}
};

static const char *(bad_utf8[]) = {

    // Continuation bytes.
    "\200", "\277",

    // Lonely start characters.
    "\300", "\300x", "\300xx",
    "\340", "\340x", "\340xx", "\340xxx",

    // Last byte missing.
    "\340\200", "\340\200x", "\340\200xx",
    "\357\277", "\357\277x", "\357\277xx",

    // Illegal bytes.
    "\376", "\377",

    // Overlong '/'.
    "\300\257", "\340\200\257",

    // Overlong ASCII NUL.
    "\300\200", "\340\200\200",

    // Maximum overlong sequences.
    "\301\277", "\340\237\277",

    // Illegal code positions.
    "\357\277\276", // U+FFFE
    "\357\277\277", // U+FFFF

    // UTF-16 surrogates (unpaired and paired).
    "\355\240\200",
    "\355\277\277",
    "\355\240\200\355\260\200",
    "\355\257\277\355\277\277",

    // Valid UCS-4 characters (not supported yet).
    // On systems with UCS-4 or UTF-16 wchar_t values, these
    // may eventually be supported in some fashion.
    "\360\220\200\200",
    "\370\210\200\200\200",
    "\374\204\200\200\200\200",

    NULL
};

BEGIN_TEST_CASE(TestUtf8Conversion, TestCase) {
    // Test each of our valid UTF-8 sequences.
    for (const utf8_and_wcs *good_data = good_utf8;
         good_data->utf8 != NULL;
         good_data++)
    {
        const char *utf8 = good_data->utf8;
        const wchar_t *wcs = good_data->wcs;

        // Attempt to decode the UTF-8 string.
        CHECK_EQ(utf16_from_utf8(utf8) == wcs, true);
        
        // Test the UTF-8 encoder, too.
        CHECK_EQ(utf8_from_utf16(wcs), utf8);
    }
    
    // Test each of our illegal UTF-8 sequences.
    for (const char **bad_data = bad_utf8; *bad_data != NULL; bad_data++) {
        const char *utf8 = *bad_data;
        
        // Attempt to decode the UTF-8 string.
        CHECK_THROWN(std::exception, utf16_from_utf8(utf8));
    }    
} END_TEST_CASE(TestUtf8Conversion);

#endif // BUILD_TEST_CASES
